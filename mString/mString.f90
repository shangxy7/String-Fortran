!
!编制(Author) : SHANG, Xinyuan
!邮箱(Email)  : x.y.shang@hotmail.com
!时间(Date)   : 2018年5月7日, May 7,2018
!功能(Func.)  : 自定义字符串类型, define String type
!成员函数(function members)：
!  logical:isEmpty(),
!  uppercase(),
!  lowercase(),
!  adjust(integer:width, integer:how),
!  integer:split(String:strs(:), character(len=*):sep), 
!  logical:isVarName(),
!  integer:typeof(), 
!  join(strs),
!  replace(old,new), 
!  tab2blanks(integer:length),
!  String: reverse(),
!  toNum(var),
!
!
module mString
implicit none
private::isEmpty, uppercase, lowercase,&
  eq_chars, eq_string, ne_string, ne_chars,&
  adjust, adjustLeft, adjustRight, adjustCenter,&
  split, isVarName, typeof, join, replace, tab2blanks,&
  reverse, toNum_real, toNum_integer, toNum_logical
  
type:: String
  character(len=:),allocatable::s
  contains
  procedure,pass::isEmpty
  generic::eq => eq_chars, eq_string
    procedure,pass::eq_chars
    procedure,pass::eq_string
  generic::ne => ne_chars, ne_string
    procedure,pass::ne_chars
    procedure,pass::ne_string
  procedure,pass::uppercase
  procedure,pass::lowercase
  procedure,pass::adjust
  procedure,pass::split
  procedure,pass::isVarName
  procedure,pass::typeof
  procedure,pass::join
  procedure,pass::replace
  procedure,pass::tab2blanks
  procedure,pass::reverse
  generic::toNum => toNum_real, toNum_integer, toNum_logical
    procedure,pass::toNum_real
    procedure,pass::toNum_integer
    procedure,pass::toNum_logical
end type

interface assignment(=)
  module procedure string_assign_chars
  module procedure chars_assign_string
  module procedure stringArr_assign_charsArr
end interface

interface operator(+)
  module procedure string_add_string
  module procedure string_add_chars
  module procedure chars_add_string
  module procedure chars_add_chars
end interface

interface operator(//)
  module procedure string_add_string
  module procedure string_add_chars
  module procedure chars_add_string
  !module procedure chars_add_chars
end interface

interface operator(==)
  module procedure string_eq_string
  module procedure string_eq_chars
  module procedure chars_eq_string
  !module procedure chars_eq_chars
end interface

interface operator(/=)
  module procedure string_ne_string
  module procedure string_ne_chars
  module procedure chars_ne_string
  !module procedure chars_ne_chars
end interface

interface operator(.in.)
  module procedure string_in_charsArr
  module procedure chars_in_stringArr
  module procedure string_in_stringArr
  module procedure chars_in_charsArr
end interface

interface operator(.notIn.)
  module procedure string_notIn_charsArr
  module procedure chars_notIn_stringArr
  module procedure string_notIn_stringArr
  module procedure chars_notIn_charsArr
end interface

  contains
  
subroutine string_assign_chars(left,right)
type(String),intent(out)::left
character(len=*),intent(in)::right
  left%s = right
end subroutine

subroutine chars_assign_string(left,right)
character(len=*),intent(out)::left
type(String),intent(in)::right
  left = ''
  left = right%s
end subroutine

subroutine stringArr_assign_charsArr(left,right)
type(String),intent(out)::left(:)
character(len=*),intent(in)::right(:)
integer::n,i
  n = min(size(left),size(right))
  forall(i=1:n) left(i)%s = trim(right(i))
end subroutine

type(String) function string_add_string(a,b)
type(String),intent(in)::a,b
  string_add_string%s = a%s // b%s
end function
  
type(String) function string_add_chars(a,b)
type(String),intent(in)::a
character(len=*),intent(in)::b
  string_add_chars%s = a%s // b
  end function
  
type(String) function chars_add_string(a,b)
character(len=*),intent(in)::a
type(String),intent(in)::b
  chars_add_string%s = a // b%s
  end function
  
type(String) function chars_add_chars(a,b)
character(len=*),intent(in)::a,b
  chars_add_chars%s = a // b
end function
  
logical function string_eq_string(left,right)
type(String),intent(in)::left,right
integer::lLen,rLen
  lLen = 0
  rLen = 0
  if(.not.allocated(left%s)) lLen = 0
  if(.not.allocated(right%s)) rLen = 0
  lLen = len(left%s)
  rLen = len(right%s)
  
  if(lLen==0 .and. rLen==0) then
    string_eq_string = .True.
    return
  endif
  
  if(lLen /= rLen) then
    string_eq_string = .false.
    return
  endif
  
  string_eq_string = .False.
  if(left%s==right%s) string_eq_string = .true.
end function
  
logical function string_eq_chars(left,right)
type(String),intent(in)::left
character(len=*),intent(in)::right
type(String)::rightIn
  rightIn%s = right
  string_eq_chars = string_eq_string(left,rightIn)
end function
  
logical function chars_eq_string(left,right)
character(len=*),intent(in)::left
type(String),intent(in)::right
  chars_eq_string = string_eq_chars(right,left)
  end function
  
  
logical function string_ne_string(left,right)
type(String),intent(in)::left,right
character(len=:),allocatable::a,b
  string_ne_string = .not.(string_eq_string(left,right))
end function
  
logical function string_ne_chars(left,right)
type(String),intent(in)::left
character(len=*),intent(in)::right
  string_ne_chars = .not.(string_eq_chars(left,right))
end function
  
  
logical function chars_ne_string(left,right)
character(len=*),intent(in)::left
type(String),intent(in)::right
  chars_ne_string = .not.(chars_eq_string(left,right))
  end function
  
  
logical function string_in_charsArr(left,right)
type(String),intent(in)::left
character(len=*),intent(in)::right(:)
integer::i
string_in_charsArr = .False.
  do i=1,size(right)
    if(left==right(i))then
      string_in_charsArr = .True.
      return
    endif
  enddo
  end function
  
logical function chars_in_stringArr(left,right)
character(len=*),intent(in)::left
type(String),intent(in)::right(:)
integer::i
chars_in_stringArr = .False.
  do i=1,size(right)
    if(left==right(i))then
      chars_in_stringArr = .True.
      return
    endif
  enddo
  end function
  
logical function string_in_stringArr(left,right)
type(String),intent(in)::left,right(:)
integer::i
string_in_stringArr = .False.
  do i=1,size(right)
    if(left%s==right(i)%s)then
      string_in_stringArr = .True.
      return
    endif
  enddo
  end function
  
logical function chars_in_charsArr(left,right)
character(len=*),intent(in)::left,right(:)
integer::i
chars_in_charsArr = .False.
  do i=1,size(right)
    if(left==right(i))then
      chars_in_charsArr = .True.
      return
    endif
  enddo
  end function
  
  
logical function string_notIn_charsArr(left,right)
type(String),intent(in)::left
character(len=*),intent(in)::right(:)
  string_notIn_charsArr = .not.(string_in_charsArr(left,right))
  end function
  
logical function chars_notIn_stringArr(left,right)
character(len=*),intent(in)::left
type(String),intent(in)::right(:)
  chars_notIn_stringArr = .not.(chars_in_stringArr(left,right))
  end function
  
logical function string_notIn_stringArr(left,right)
type(String),intent(in)::left,right(:)
  string_notIn_stringArr = .not.(string_in_stringArr(left,right))
  end function

logical function chars_notIn_charsArr(left,right)
character(len=*),intent(in)::left,right(:)
  chars_notIn_charsArr = .not.(chars_in_charsArr(left,right))
  end function

!
!============================================
!依规则判断字符串是否相等，tell if string equeal strIn. by regulation
!============================================
logical function eq_string(self,strIn,re)
class(String)::self
type(String),intent(in)::strIn
character(len=*),optional,intent(in)::re
type(String)::a,b,reIn
  !handling re
  if(.not.present(re)) reIn=''
  reIn%s = re
  reIn%s = reIn%uppercase()
  
  a = self
  b = strIn
  if(index(reIn%s,'I')/=0)then
    a = a%uppercase()
    b = b%uppercase()
  endif

  if(index(reIn%s,'R')/=0)then
    a = trim(a%s)
    b = trim(b%s)
  endif
  
  if(index(reIn%s,'L')/=0)then
    a = a%reverse()
    a = trim(a%s)
    a = a%reverse()
    b = b%reverse()
    b = trim(b%s)
    b = b%reverse()
  endif
  
  eq_string = .False.
  if(len(a%s)/=len(b%s))return
  if(a%s==b%s) eq_string = .true.
end function
  
logical function eq_chars(self,charsIn,re)
class(String)::self
character(len=*),intent(in)::charsIn
character(len=*),optional,intent(in)::re
type(String)::strIn
  strIn%s = charsIn
  eq_chars = eq_string(self,strIn,re=re)
  end function
  
logical function ne_string(self,strIn,re)
class(String)::self
type(String),intent(in)::strIn
character(len=*),optional,intent(in)::re
  ne_string = .not.eq_string(self,strIn,re)
end function
  
logical function ne_chars(self,charsIn,re)
class(String)::self
character(len=*),intent(in)::charsIn
character(len=*),optional,intent(in)::re
  ne_chars = .not.eq_chars(self,charsIn,re=re)
end function
  
!
!============================================
!判断字符串是否为空，tell if string is empty.
!============================================
logical function isEmpty(self)
class(String)::self
  isEmpty = .False.
  if(.not.allocated(self%s))then
    isEmpty = .True.
    self%s = ''
    return
  endif
  if(len(self%s)/=0) then
    isEmpty = .False.
    return
  endif
  if(self%s=='') isEmpty = .True.
end function
  
!
!============================================
!大小写, uppercase and lowercase
!============================================
function uppercase(self)
  !将字符串中所有的字母转换为大写
  !make all letters uppercase
  implicit none
  class(String)::self
  character(len=len(self%s))::uppercase
  integer::i=0

  do i=1,len(self%s)
    if( (self%s(i:i).ge.'a') .and. (self%s(i:i).le.'z') )then
      uppercase(i:i)=char( ichar(self%s(i:i))-32 )
    else
      uppercase(i:i)=self%s(i:i)
    endif
  enddo

  end function
  
  
function lowercase(self)
  !将字符串中所有的字母转换为大写
  !make all letters lowercase
  implicit none
  class(String)::self
  character(len=len(self%s))::lowercase
  integer::i=0

  do i=1,len(self%s)
    if( (self%s(i:i).ge.'A') .and. (self%s(i:i).le.'Z') )then
      lowercase(i:i)=char( ichar(self%s(i:i))+32 )
    else
      lowercase(i:i)=self%s(i:i)
    endif
  enddo

  end function
  
!
!============================================
!字符串对齐, align string in appointed width
!============================================
function adjust(self,width,how)
!width: output string width
!how:-1(left alignment),0(center alignment),1(right alignment)
implicit none
class(String)::self
integer,intent(in)::width
integer,optional,intent(in)::how
character(len=width)::adjust
integer::howIn
  if(present(how))then
    howIn = how
  else
    howIn = 0
  endif
  
  if(len(self%s) >= width)then
    adjust = self%s(1:width)
    return
  endif
  
  if(howIn<0)then
    adjust = adjustLeft(self%s,width)
  elseif(howIn==0)then
    adjust = adjustCenter(self%s,width)
  else
    adjust = adjustRight(self%s,width)
  endif
end function

function adjustCenter(str, width, fill)
!将字符串在指定宽度内居中对齐
implicit none
character(len=*),intent(in)::str
integer,intent(in)::width
character,optional,intent(in)::fill
character(len=width)::adjustCenter
character::filling
integer:: strLen, blanks
  filling = ' '
  if(present(fill)) filling = fill
  
  strLen = len(str)
  if(strLen > width)then
    adjustCenter = str(strLen-width:)
  else
    blanks = (width - strLen)/2
    adjustCenter = repeat(filling,blanks) // str // repeat(filling,blanks)
  endif
end function

function adjustLeft(str, width)
!将字符串在指定宽度内左对齐
implicit none
character(len=*),intent(in)::str
integer,intent(in)::width
character(len=width)::adjustLeft
  adjustLeft = repeat(' ',width)
  adjustLeft(1:len(trim(adjustl(str)))) = str
end function

function adjustRight(str, width)
!将字符串在指定宽度内右对齐
implicit none
character(len=*),intent(in)::str
integer,intent(in)::width
character(len=width)::adjustRight
  adjustRight = repeat(' ',width)
  adjustRight(width-len(trim(adjustl(str)))+1:width) = str
  end function
  
!
!============================================
!分割字符串, split string
!============================================
integer function split(self,strs,sep) result(count)
!输入字符串，输出string类型的字符串数组
implicit none
class(String)::self
type(string),intent(out)::strs(:)
character(len=*),optional::sep
!
character(len=:),allocatable::s
integer::inx
  count = 0
  
  if(trim(adjustl(self%s))=='')then
  !对于空字符串
    count = 0
    return
  endif
    
  if(.not.present(sep).or.sep==' ')then
  !空格分隔（默认分隔方式）
    s = trim(adjustl(self%s))
    do while(s/='')
      inx = index(s,' ')
      if(inx==0)then
        count = count + 1
        strs(count)%s = s
        return
      else
        count = count + 1
        strs(count)%s = s(1:inx-1)
        s = trim(adjustl(s(inx+1:)))
        cycle
      endif
    enddo
    
  else
  !指定分隔符集合
    s = trim(adjustl(self%s))
    do while(s/='')
      do inx = 1,len(s)
        if(index(sep,s(inx:inx)) .ne. 0)then
          count = count + 1
          strs(count)%s = s(1:inx-1)
          if(strs(count)%s=='') count = count-1
          s = trim(adjustl(s(inx+1:)))
          exit
        else
          if(inx==len(s))then
            count = count + 1
            strs(count)%s = s(1:inx)
            if(strs(count)%s=='') count = count-1
            return
          else
            cycle
          endif  
        endif
      enddo
    enddo
  endif
  end function
  
!
!============================================
! 判段某字符串是否可作为变量名
!============================================
logical function isVarName(self)
! 1.不含特殊字符(包括空格)
! 2.首字符必须是字母
class(String)::self
  isVarName = (scan(self%s,'[]/!@#$%&() =}{";:\|.><') == 0 )
  isVarName = isVarName .and. (scan(self%s,'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')==1)
end function
  
!
!============================================
!判段某字符串是否为数值
!============================================
integer function typeof(self)
!from: 
! Verify that a character string represents a numerical value
! 确定字符是否是数值类型：
! 0-非数值的字符串
! 1-整数(integer)
! 2-小数(fixed point real)
! 3-指数类型实数(exponent type real)
! 4-双精度实数指数形式(exponent type double)
! 5-逻辑类型(logical data)
! 6-变量名(variable name)
implicit none
class(String)::self
type(String)::strIn
integer :: num, nmts, nexp, kmts, ifexp, ichr
integer, parameter :: kint = 1 ! integer
integer, parameter :: kfix = 2 ! fixed point real
integer, parameter :: kexp = 3 ! exponent type real
integer, parameter :: kdbl = 4 ! exponent type double

strIn = self%s

! initialise
num = 0  ! 数字的格式，最后传递给typeof返回
nmts = 0 ! 整数或浮点数的数字个数
nexp = 0 ! 指数形式的数字个数
kmts = 0 ! 有+-号为1，否则为0
ifexp = 0 ! 似乎没用
! loop over characters
ichr = 0

if(strIn%upperCase()=='T'.or.strIn%upperCase()=='F')then
  !logical data
  typeof=5
  return
elseif(strIn%isVarName())then
  !variable name
  typeof=6
  return
endif

do
  If (ichr>=len(self%s))then
    ! last check
    If (nmts==0)exit
    If (num>=kexp .And. nexp==0)exit
    typeof = num
    return
  endif
  ichr = ichr + 1
  select case (self%s(ichr:ichr))
    ! process blanks
  case (' ')
    continue
    ! process digits
  case ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    If (num==0) num = kint
    If (num<kexp) Then
      nmts = nmts + 1
      ! 整数或浮点数+1
    else
      nexp = nexp + 1
      ! 指数形式+1
    endif
    ! process signs
  case ('+', '-')
    If (num==0) Then
      If (kmts>0) Exit
      ! 出现2个符号，非数字
      kmts = 1
      num = kint
    else
      if (num<kexp) exit
      if (ifexp>0) exit
      ifexp = 1
    endif
    ! process decimal point
  case ('.')
    if (num/=kint .and. ichr/=1) exit
    ! 前面不是整数，小数点也不是第一个字符，则非数字
    num = kfix
    ! process exponent
  case('e', 'E')
    if (num>=kexp) exit
    if (nmts==0) exit
    num = kexp

  case('d', 'D')
    if (num>=kexp) exit
    if (nmts==0) exit
    num = kdbl
    ! any other character means the string is non-numeric
    case default
    exit
  end select
enddo
! if this point is reached, the string is non-numeric
typeof = 0
  end function
  
!============================================
!字符串拼接，concatenate string array with seperators
!============================================
function join(self,strs)
class(String)::self
type(String),intent(in)::strs(:)
type(String)::join
integer::i,rows
  join%s = ''
  rows = size(strs)
  if(rows>=1) join=strs(1)
  if(rows==1) return
  do i=2,rows
    join%s = join%s // self%s // strs(i)%s
  enddo
end function
  
  
!============================================
!子字符串替换, replace sub. string old with sub. string new
!============================================
function replace(self,oldIn,newIn)
implicit none
class(String)::self
character(len=*),intent(in)::oldIn,newIn
type(String)::replace,mirror
integer::i,oldLen,cc
logical::switch
  replace%s = ''
  if(self%isEmpty()) return
  
  mirror%s = repeat('1',len(self%s))
  oldLen = len(oldIn)
  do i=1,len(self%s)-oldLen+1
    if(mirror%s(i:i)=='0') cycle
    if(self%s(i:i-1+oldLen)==oldIn)then
      mirror%s(i:i-1+oldLen) = repeat('0',oldLen)
    endif
  enddo

  switch = .False.
  if(mirror%s(1:1)=='0') switch = .True.
  do i=1,len(mirror%s)
    if(mirror%s(i:i)=='1') then
      replace%s = replace%s // self%s(i:i)
      switch = .True.
    endif
    
    if(mirror%s(i:i)=='0') then
      if(switch)then
        cc = 1
        replace = replace + newIn
        switch = .False.
      else
        cc = cc + 1
        if(cc==oldLen) switch = .True.
      endif
    endif
  enddo
  
  end function
  
!============================================
!将tab键转换为对应的空格, change tab to blanks
!============================================
function tab2blanks(self,length)
class(String)::self
integer::length
type(String)::tab2blanks
  tab2blanks = self%replace(char(9),repeat(' ',length))
  end function
  
!============================================
!将字符串逆序排列, reverse string
!============================================
type(String) function reverse(self)
class(String)::self
character(len=len(self%s))::mirror
integer::sLen,i,j
  if(self%isEmpty()) return
  sLen = len(self%s)
  reverse%s = ''
  
  do i=1,sLen
    j = sLen+1-i
    mirror(j:j) = self%s(i:i)
  enddo
  reverse%s = mirror
  end function
  

!============================================
!将tab键转换为对应的空格, change tab to blanks
!============================================
integer function toNum_real(self,var) result(iErr)
class(String)::self
real(8),intent(out)::var
  iErr = 1
  read(self%s,*,ioStat=iErr) var
  if(iErr/=0) iErr = 1
  end function

integer function toNum_integer(self,var) result(iErr)
class(String)::self
integer,intent(out)::var
  iErr = 1
  read(self%s,*,ioStat=iErr) var
  if(iErr/=0) iErr = 1  
  end function
  
integer function toNum_logical(self,var) result(iErr)
class(String)::self
logical,intent(out)::var
type(String)::selfIn
  selfIn = trim(adjustl(self%s))
  selfIn = self%uppercase()
  
  iErr = 1
  if(selfIn%s=='T')then
    var = .True.
    iErr = 0
  elseif(selfIn%s=='F')then
    var = .False.
    iErr = 0
  endif
end function
  
end module