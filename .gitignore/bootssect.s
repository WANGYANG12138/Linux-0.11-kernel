!本程序采用as86汇编语法。




! SYS_SIZE is the number of clicks (16 bytes) to be loaded.
! 0x3000 is 0x30000 bytes = 196kB, more than enough for current
! versions of linux
!设置编译链接后system模块的内存占用为SYSSIZE，即为0x3000节（一节为16字节）
SYSSIZE = 0x3000
!
!	bootsect.s		(C) 1991 Linus Torvalds
!
! bootsect.s is loaded at 0x7c00 by the bios-startup routines, and moves
! iself out of the way to address 0x90000, and jumps there.
!
! It then loads 'setup' directly after itself (0x90200), and the system
! at 0x10000, using BIOS interrupts. 
!
! NOTE! currently system is at most 8*65536 bytes long. This should be no
! problem, even in the future. I want to keep it simple. This 512 kB
! kernel size should be enough, especially as this doesn't contain the
! buffer cache as in minix
!
! The loader has been made as simple as possible, and continuos
! read errors will result in a unbreakable loop. Reboot by hand. It
! loads pretty fast by getting whole sectors at a time whenever possible.


!定义了三个段(.text .data .bss) 和6个全局标识符
.globl begtext, begdata, begbss, endtext, enddata, endbss
.text
begtext:
.data
begdata:
.bss
begbss:



.text        ！表示下面的代码都是正文段(代码段)

SETUPLEN = 4				! nr of setup-sectors
BOOTSEG  = 0x07c0			! original address of boot-sector
INITSEG  = 0x9000			! we move boot here - out of the way
SETUPSEG = 0x9020			! setup starts here
SYSSEG   = 0x1000			! system loaded at 0x10000 (65536).
ENDSEG   = SYSSEG + SYSSIZE		! where to stop loading

! ROOT_DEV:	0x000 - same type of floppy as boot.
!		0x301 - first partition on first drive etc
ROOT_DEV = 0x306

entry start
start:
	mov	ax,#BOOTSEG
	mov	ds,ax
	mov	ax,#INITSEG
	mov	es,ax
	mov	cx,#256
	sub	si,si
	sub	di,di
	rep
	movw                ！每次移动一个字(Word)
	jmpi	go,INITSEG

!ds,es,as,ss都为段寄存器;sp为堆栈指针寄存器（指向栈顶的地址）	

go:	mov	ax,cs               !将数据段和附加段寄存器中指向的段都为：0x9000（指向一个段）
	mov	ds,ax               !即想要对该段的数据/堆栈进行操作,必须先对相应的段寄存器赋值
	mov	es,ax
! put stack at 0x9ff00.
	mov	ss,ax               !将堆栈段寄存器指向段：0x9000
	mov	sp,#0xFF00		    ! arbitrary value >>512

! load the setup-sectors directly after the bootblock.
! Note that 'es' is already set up.

!這段代码的作用：利用BIOS中断INT 0X13将setup模块从磁盘的第二个扇区开始读到0x90200,共读4个扇区
!若读错，则复位驱动并重试
load_setup:
!在使用BIOS中断时，必须先设置参数（参数设置在通用寄存器中）
!75~78行是对BIOS中断13(即INT 13)的定义:
!AX的AH--指定功能号,AL--当AH=2时,AL为需要读出的扇区数
!BX存放读取内存的缓冲地址(ES:BX)
!CX的CL高2位+CH=柱面号,CL低6位=扇区号
!DX的DH--指定磁头号,DL--指定驱动器号
	mov	dx,#0x0000		! drive 0, head 0
	mov	cx,#0x0002		! sector 2, track 0
	mov	bx,#0x0200		! address = 512, in INITSEG;存放读取内存的缓冲地址(ES:BX)
	mov	ax,#0x0200+SETUPLEN	! service 2, nr of sectors
	int	0x13			! read it;使用软件中断命令开启中断

	jnc	ok_load_setup		! ok - continue;没有发生错误则继续跳转到ok_load_setup

	mov	dx,#0x0000
	mov	ax,#0x0000		! reset the diskette
	int	0x13
	j	load_setup      !即jmp指令；跳转到load_setup

!这段代码的作用是：取磁盘驱动器的参数,尤其是每个磁道的扇区数
ok_load_setup:

!Get disk drive parameters, specifically nr of sectors/track

	mov	dl,#0x00        !DL指定驱动器号
	mov	ax,#0x0800		! AH=8 is get drive parameters;AH指定功能
	int	0x13            !注意：调用中断会产生返回值（返回值放到相应的寄存器中）
	mov	ch,#0x00
	seg cs
	mov	sectors,cx      !CX中存放的是每磁道的扇区数（是中断的返回值）
	mov	ax,#INITSEG
	mov	es,ax           !取磁盘参数改变了ES的值,这里重新改回

! Print some inane message
!这一个中断代表：读光标位置操作
	mov	ah,#0x03		! read cursor pos;读光标位置
	xor	bh,bh           !xor指令表示将bh与bh进行异或,将结果存到第一个bh(bh代表页号)
	int	0x10
!这一个中断代表：显示字符串到光标位置(屏幕上),并移动光标到结尾处
	mov	cx,#24          !显示24个字符
	mov	bx,#0x0007		! page 0, attribute 7 (normal)
	mov	bp,#msg1        !bp寄存器指向要显示的字符串的起始位置
	mov	ax,#0x1301		! write string, move cursor
	int	0x10

! ok, we've written the message, now
! we want to load the system (at 0x10000)
!这一段代码表示将system模块加载到内存的0x010000

	mov	ax,#SYSSEG  !#SYSSEG为要加载到内存的地址
	mov	es,ax		! segment of 0x010000;es存放system模块的段地址 
	call	read_it !调用read_it--将system模块加载到内存中
	call	kill_motor  !关闭驱动器

! After that we check which root-device to use. If the device is
! defined (!= 0), nothing is done and the given device is used.
! Otherwise, either /dev/PS0 (2,28) or /dev/at0 (2,8), depending
! on the number of sectors that the BIOS reports currently.

	seg cs
	mov	ax,root_dev
	cmp	ax,#0
	jne	root_defined
	seg cs
	mov	bx,sectors
	mov	ax,#0x0208		! /dev/ps0 - 1.2Mb
	cmp	bx,#15
	je	root_defined
	mov	ax,#0x021c		! /dev/PS0 - 1.44Mb
	cmp	bx,#18
	je	root_defined
undef_root:
	jmp undef_root
root_defined:
	seg cs
	mov	root_dev,ax

! after that (everyting loaded), we jump to
! the setup-routine loaded directly after
! the bootblock:

	jmpi	0,SETUPSEG

! This routine loads the system at address 0x10000, making sure
! no 64kB boundaries are crossed. We try to load it as fast as
! possible, loading whole tracks whenever we can.
!
! in:	es - starting address segment (normally 0x1000)
!
sread:	.word 1+SETUPLEN	! sectors read of current track;当前磁道的已读扇区数
head:	.word 0			    ! current head;当前磁头号
track:	.word 0			    ! current track;当前磁道号

read_it:
	mov ax,es           !将es的值0x1000给ax
	test ax,#0x0fff     !text指令表示对两个数进行与操作：若结果为0,则标志位ZF置位;否则标志位置0 
die:	jne die			! es must be at 64kB boundary;判断ZF是否等于0,等于则无限循环,否则向下执行
	xor bx,bx		    ! bx is starting address within segment;清空bx的内容(bx作为段偏移量)
!判断是否将system模块的代码全部读入到内存	
rp_read:
	mov ax,es
	cmp ax,#ENDSEG		! have we loaded all yet?;判断是否读到system结尾的地址
	jb ok1_read         !ax(目标操作数)小于#ENDSEG源操作数,则跳转(到ok1_read);否则继续向下执行(即返回到call指令的下一句)
	ret
!确定了AL的值(可以读取的扇区数,而不会段溢出)
ok1_read:
	seg cs
	mov ax,sectors      !将当前使用的软盘的当前磁道扇区数给寄存器AX
	sub ax,sread        !当前磁道的扇区数减当前已读过的扇区数,结果为AX
	mov cx,ax            
	shl cx,#9           !逻辑左移指令,对CX中的内容进行左移9位(对CX的内容乘以2^9)
	add cx,bx           !BX+CX的值为该段在将所有扇区加载到内存的该段后,所具有的字节总数（注意：此时还未加载）
	jnc ok2_read        !进位位没置位则跳转(此处若没超过段最大长64KB,则跳转到ok2_read)
	je ok2_read         !0标志位(ZF)为1,则跳转。ZF零标志 – 如果运算结果是零则置位，否则复位。         
	xor ax,ax              
	sub ax,bx
	shr ax,#9 

!根据AL的值,使用read_track读取一个磁道的数据;若没有读完,则根据磁头号不同,执行不同语句
ok2_read:
	call read_track     !读取软盘中的system模块的数据
	mov cx,ax           !AX为read_track的返回值,CX中为该次操作后读取的扇区数
	add ax,sread        !
	seg cs
	cmp ax,sectors      !若还有扇区未读,跳转到ok3_read
	jne ok3_read
	mov ax,#1           !若该磁道扇区已读完,则向下执行
	sub ax,head
	jne ok4_read       
	inc track           !磁道号加一

!重新赋值磁头head和扇区的起始地址AX
ok4_read:
	mov head,ax        ！head中存放磁头号
	xor ax,ax       

ok3_read:            
	mov sread,ax       
	shl cx,#9
	add bx,cx          !确定BX的值,没有溢出,则跳转回rp_read
	jnc rp_read
	mov ax,es
	add ax,#0x1000     
	mov es,ax          !否则段基址es+=0x1000,将数据存入下一个段,再重新循环
	xor bx,bx
	jmp rp_read

!读当前磁道上的指定开始扇区的扇区数的数据到ES:BX开始处
!AX的AH--指定功能号,AL--当AH=2时,AL为需要读出的扇区数
!BX存放读取内存的缓冲地址(ES:BX)
!CX的CL高2位+CH=柱面号,CL低6位=扇区号
!DX的DH--指定磁头号,DL--指定驱动器号
read_track:
!将AX、BX、CX、DX的数据进行保护(压栈)
	push ax
	push bx
	push cx
	push dx
	mov dx,track    !当前磁道号
	mov cx,sread    !当前已读的扇区数
	inc cx          !inc指令为加一指令
	mov ch,dl       !CH中存放当前磁道号
	mov dx,head     !软盘只有磁头0和磁头1
	mov dh,dl       !DH为磁头号
	mov dl,#0       !DL为驱动器号
	and dx,#0x0100  !始终保证磁头号不大于1
	mov ah,#2   
	int 0x13        !使用BIOS中断读取对应数据到ES:BX位置开始处
	jc bad_rt       !若读取数据出错,则跳转到bad_rt
!将AX、BX、CX、DX的数据还原到相应段寄存器(弹栈)
	pop dx
	pop cx
	pop bx
	pop ax
	ret             
!若读磁盘操作出错,则执行驱动器复位操作(磁盘中断功能号0);再跳转回read_track

bad_rt:	mov ax,#0
	mov dx,#0
	int 0x13
	pop dx
	pop cx
	pop bx
	pop ax
	jmp read_track

/*
 * This procedure turns off the floppy drive motor, so
 * that we enter the kernel in a known state, and
 * don't have to worry about it later.
 */
!关闭软盘
kill_motor:
	push dx
	mov dx,#0x3f2
	mov al,#0
	outb
	pop dx
	ret
！下面这句为伪指令(汇编命令),句型组成为：  标号名:  .汇编命令
sectors:    
	.word 0           !.word表示：即在当前指令位置(即标号sectors的值---也就是地址)放一个值(值为0)  

msg1:
	.byte 13,10       !.byte表示：定义0个/多个单字节数据(每个数据由逗号隔开)
	.ascii "Loading system ..."   !从位置计数器所指当前位置分配存储空间并保存字符串(0个/多个字符串由逗号隔开);地址连续并且每个字符串末尾不会自动添加NULL
	.byte 13,10,13,10

.org 508
root_dev:
	.word ROOT_DEV
boot_flag:
	.word 0xAA55


!.text   表示代码段(正文段)
!.data   表示初始化数据段
!.bss    表示未初始化数据段

.text      
endtext:
.data
enddata:
.bss
endbss:
