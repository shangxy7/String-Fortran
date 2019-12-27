[ENGLISH](## ENGLISH)

## 目录

- [基本介绍和约定](## 基本介绍和约定)
- [String 类型说明](## String 类型说明)
  - [运算符](### 运算符)
    - [赋值（=）](### 赋值（=）)
    - [字符串拼接（+，//）](### 字符串拼接（+，//）)
    - [包含（.in. ,  .notIn.）](### 包含（.in. ,  .notIn.）)

  - [String方法](## String方法)
    - [isEmpty](### isEmpty)

    - [eq 和 ne](### eq 和 ne)

    - [uppercase 和 lowercase](### uppercase 和 lowercase)

    - [adjust](### adjust)

    - [split](### split)

    - [join](### join)

    - [typeof](### typeof)

    - [replace](### replace)

    - [tab2blanks](### tab2blanks)

    - [reverse](### reverse)

    - [toNum](### toNum)

  - [综合范例](## 综合范例)
    - [读取 CSV 文件数据](### 读取 CSV 文件数据)


## 基本介绍和约定

String 类型是使用Fortran 语言自定义的字符串类型，它重载了常见的赋值和判断符号，并绑定了常用的字符串处理函数，以弥补Fortran 语言在字符串处理方面的弱势。

Fortran 语法允许使用固定长度的字符串类型，其声明方法为 `character(len=10)::chars` ，但该方法声明的字符串长度固定，并不利于进行字符串扩充、比较、修改等常规操作。另外，该方法声明的字符串数组长度均一致，不适合不定长度字符串的处理需求。本模块mString 所定义的String 类型，使用allocatable 关键词，封装递延类字符串，字符串长度、内容不受约束，可轻松解决上述问题。在本说明文档中，为方便区分，将本模块定义的字符串类型统称为**String 类型**，而将固定长字符串类型统称为**Chars 类型**。

## String 类型说明

String 类型中封装了唯一的属性参数 `s`，若干方法（为方便使用，所有方法均为函数function，并有返回值），并重载了 `=, +, //, ==, /=, .in., .notIn.` 运算符，实例化之后方可使用。String 类型封装在mString 模块中，其基本结构如下：

```fortran
!mString.f90
module mString
    type:: String
      character(len=:),allocatable::s
      contains
      procedure,pass::function_name
    end type

    contains

    function function_name()
    ...
    end function
end module
```

String 类型在正式使用之前需依次经过引用、创建、初始化三个步骤。其中，初始化并非必要，但建议最好使用该步骤。

```fortran
!Samples.f90
  program samples
  !1.引用，citing
  use mString

  !2.创建，creating
  type(String)::str

  !3.初始化，assignment
  str = ''
  
  ...
  end program
```

### 运算符

#### 赋值（=）

String 类型重载了一个赋值运算=，可使用固定长度字符串chars 类型、String类型直接赋值，也可在chars类型和string类型之间相互赋值。当chars 类型的长度低于String 类型的长度时，字符串将会被截断。

另外，固定长度字符串数组可以使用 = 直接赋值，String 类型也重载了String数组赋值功能。

```fortran
  type(String)::strA,reactors(5)
  character(len=10)::chars,charsArr(5)
  
  strA = 'This is string type...'
  chars = strA
  print*, strA
  print*, chars
  
  reactors = ['pwr','bwr','vver','wwer','candu']
  charsArr = ['pwr','bwr','vver','wwer','candu']
  print*, reactors	
  print*, charsArr
```

![image-20191226151723522](.\pics\image-20191226151723522.png)

#### 字符串拼接（+，//）

在Fortran语法中，// 是字符串拼接符号，String 类型重载了 + 和 //，可实现 chars类型、String类型、不可变字符串之间的拼接。

```fortran
  type(string)::cladding,pellet,reactor
  character(len=3)::left, right
  
  reactor = 'bwr'
  pellet = 'uo2'
  cladding = 'zr4'
  left  = ' { '
  right = ' { '
  
  print*, '!' + reactor + ' = ' // left // cladding + ' + ' + pellet // right
  !输出：!bwr =  { zr4 + uo2  } 
```

#### 字符串比较（==, /=）

String 类型重载了 ==，/=，可直接与固定长字符串进行比较。Fortran语法中，字符串末端的空格不在比较之列，即不同长度字符串有可能相等，例如：

```fortran
print*, '   pwr'=='pwr', 'pwr   '=='pwr'
!输出：F T
```

在String 类型中，末尾的空格一样有意义，因此，任何字符串在与String类型进行比较时，不同长度的字符串的比较结果为假。同时，String 类型并不改变Fortran中两个固定长度字符串的比较规则，即引入mString模块后，\'pwr      \'==\'pwr\' 的结果依然为真。

```fortran
  type(String)::strA,strB
  
  print*, '   pwr'=='pwr', 'pwr   '=='pwr'
  strA =  '   pwr'
  strB =  'pwr'
  print*, strA == strB, strA /= strB
  print*, '   pwr' == strB, strA == 'pwr'
  !输出：
  !F T
  !F T
  !F F
```

*ps. String 类型中另外绑定了两个字符串比较函数，[eq和ne](####eq 和 ne)，可通过参数指定比较时是否忽略大小写、前后端空格，大大提高了字符串比较的便利*

#### 包含（.in. ,  .notIn.）

String类型针对字符串数组自定义了两个包含关系型判断，.in. 和  .notIn.，以判断符号左边的字符串是否在被右端的字符串数组包含。字符串判断的标准和 ==、/=相同，[点击可查看详情](###字符串比较（\==, /=）)。

```fortran
  type(string)::reactor,reactors(5)
  character(len=10)::chars, charsArr(5)
  
  reactor = 'bwr'
  chars = 'pwr'
  reactors = ['pwr','bwr','vver','wwer','candu']
  charsArr = ['pwr','bwr','vver','wwer','candu']
  
  print*, 'pwr'.in.reactors,      'pwr'.notIn.reactors
  print*, 'PWR'.in.reactors,      'PWR'.notIn.reactors
  print*, chars.in.charsArr,      chars.notIn.charsArr
  print*, reactor.in.reactors, reactor.notIn.reactors
  print*, reactor.in.charsArr, reactor.notIn.charsArr
  !输出
  !T F
  !F T
  !T F
  !T F
  !F T
```

### String方法

#### isEmpty

*格式：str%isEmpty()*
*返回值：logical*

String 类型的属性 s 采用allocatable定义，若未对其进行初始化，s 的默认指向为空，在字符串比较时容易产生运行时错误，因此，String 绑定了isEmpty方法，替代 isEmpy%s == '' 的判断，若String声明的变量指向为空，则自动将其修改为长度为0的字符串，不影响其它正常使用。

```fortran
  type(String)::empty
  
  print*, empty%isEmpty()	!T
  empty = ''
  print*, empty%isEmpty()	!T
  empty = ' '
  print*, len(empty%s), empty%s=='', empty == ''	!1 T F
  print*, empty%isEmpty()	!F
```

**从倒数第二行可以看出，直接使用 empty%s=='' 会得到事与愿违的结果，这是因为，此时的比较属于两个chars类型的比较，末尾空格不在比较之列；**

#### eq 和 ne

*格式：str%eq(str1[, re='[irl]']) 	str%ne(str1[, re='[irl]'])*
*返回值：logical*
*str1   -  chars类型、String类型、字符串*
*re      -  字符串，指定比较的规则，i 代表不区分大小写，r 代表忽略右侧空格，l 代表忽略左侧空格；省略该                      参数或 re='' 时，比较方式与 ==，/= 相同*

```fortran
  type(String)::strA,strB
  
  strA =  '  pwr'
  strB = '  PWR  '
  print*, .false.,  strA%eq(strB),  strA%eq('  PWR  '),  strA%ne(strB)
  print*, .false.,  strA%eq(strB,re='i'),  strA%eq('  PWR  ',re='i'),  strA%ne(strB,re='i')
  print*, .true.,   strA%eq(strB,re='ir'), strA%eq('  PWR  ',re='ir'), strA%ne(strB,re='ir')
  print*, .true.,   strA%eq(strB,re='irl'),strA%eq('  PWR  ',re='irl'),strA%ne(strB,re='irl')
  print*, .false.,  strA%eq(strB,re='rl'), strA%eq('  PWR  ',re='rl'), strA%ne(strB,re='rl')
  !输出
  !F F F T
  !F F F T
  !T T T F
  !T T T F
  !F F F T
```

#### uppercase 和 lowercase

*格式：str%uppercase()	str%lowercase()*
*返回值：String*

将String中的字符全部转换成大写（uppercase）和小写（lowercase）。

```fortran
 type(String)::a
 
  a = 'x.y.SHANG@hotmail.com'
  print*, a%uppercase()		!X.Y.SHANG@HOTMAIL.COM
  print*, a%lowercase()		!x.y.shang@hotmail.com
  print*, a				   !x.y.SHANG@hotmail.com
```

#### adjust

*格式：str%adjust(width, how)*
*返回值：String*
*width   -   整形，输出宽度，width小于str字符串宽度时，str将会被截断输出；*
*how   -   整形，可取值为-1，0，1，分别代表左对齐，居中对齐，右对齐，默认0；*

```fortran
  type(String)::a
  
  a =     'Happy New Year'
  print*, '123456789_123456789_123456789_'
  print*, a%adjust(20)
  print*, a%adjust(20,-1)
  print*, a%adjust(20,0)
  print*, a%adjust(20,1)
  print*, a%adjust(7)
```

![image-20191227104422742](.\pics\image-20191227104422742.png)

#### split

*格式：str%split(strs, sep)*
*返回值：integer
*strs   -   输出 String 数组；*
*sep   -   分割字符集，默认空格；*

将字符串使用sep定义的字符集分割，返回分割后的段数；

```fortran
  type(String)::a, strs(20)
  integer::i
  
  a = 'x.y.SHANG@hotmail.com'
  do i=1, a%split(strs,'@.')
    print*, strs(i)
  enddo
  !输出
  !x
  !y
  !SHANG
  !hotmail
  !com
```

#### join

*格式：str%join(strs)*
*返回值：String
*strs   -   输入 String 数组；*

使用 str 字符串作为连接符，拼接 strs；

```fortran
  type(String)::reactors, a
  
  reactors = ['pwr','bwr','vver','wwer','candu']
  a = ' , '
  print*, a%join(reactors)
  !输出
  !pwr , bwr , vver , wwer , candu
```

#### typeof

*格式：str%typeof()*
返回值：integer，代表数据类型*

> *0-非数值的字符串*
> *1-整数(integer)*
> *2-小数(fixed point real)*
> *3-指数类型实数(exponent type real)*
> *4-双精度实数指数形式(exponent type double)*
> *5-逻辑类型(logical data)*
> *6-变量名(variable name)*

```fortran
  type(String)::a, strs(20)
  integer::i  
  
  a = '@cc 11 s1 0.01 .3 3d0 3e0 t F'
  do i=1, a%split(strs)
    print*, strs(i), strs(i)%typeof()
  end do
```

![image-20191227105558469](.\pics\image-20191227105558469.png)

#### replace

*格式：str%replace(old, new)*
*返回值：string
*old   -   被替代的字符串；*
*new  -   替代字符串；*

```fortran
  type(String)::a
  
  a = 'old---oldold---old'
  print*, a%replace('old','new')
  print*, a%replace('old','oldold')
  print*, a
```

![image-20191227111417746](.\pics\image-20191227111417746.png)

#### tab2blanks

*格式：str%tab2blanks(length)*
*返回值：string
*length   -   空格长度；*

将字符串中的tab键替换为长度为length的空格；

```fortran
  type(String)::a
  character(len=10)::chars
  
  open(unit=11,file='./text',status='old') !从文档中读取一行
  read(11,'(a)') chars
  a = trim(chars)
  print*, '123456789_123456789_123456789'
  print*, a%tab2blanks(20)
  print*, a
```

![image-20191227111833050](.\pics\image-20191227111833050.png)

#### reverse

*格式：str%reverse()*
*返回值：string

翻转字符串；

```fortran
  type(String)::a
  
  a = '123456789  '
  print*, a%reverse()
  print*, a
```

![image-20191227120233127](.\pics\image-20191227120233127.png)

#### toNum

*格式：str%toNum(var)*
*返回值：integer*
*var   -   输出数据；*

将字符串转化为数据，返回 0 或 1 作为错误标准，0 代表转化成功，1代表转化错误，数据由var接收；

```fortran
  type(String)::a
  integer::i, iErr, vInt
  real(8)::vReal
  logical::vLog
  
  ! 将字符串a转换为具体的数据
  a = '1 s1 1d0 t .1'
  !iErr = 1
  do i=1,a%split(strs)
    select case(strs(i)%typeof())
    case(1)
      iErr = strs(i)%toNum(vInt)
    case(2:4)
      iErr = strs(i)%toNum(vReal)
    case(5)
      iErr = strs(i)%toNum(vLog)
    case default
      iErr = 1
    end select
  end do
```



## 综合范例

### 读取 CSV 文件数据

未完待续



## ENGLISH

to be continued...