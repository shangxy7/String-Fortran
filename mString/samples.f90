
  program samples
  !1.引用，citing
  use mString
  implicit none

  !2.创建，creating
  type(String)::a,empty,strA,strB
  type(string)::cladding,pellet,reactor,reactors(5),strs(100)
  character(len=20)::chars, charsArr(5)
  integer::i, iErr, vInt
  real(8)::vReal
  logical::vLog

  !3.赋值，assignment
  a = 'Happy new year...'

  !============================================
  ! 测试：字符串状态判断
  ! 注意：强烈建议使用a%isEmpty()函数来判断String类型是否为空，而不是使用a == '', 这样及有可能带来运行时错误
  ! test: tell string status
  ! note: a%isEmpty() is strongly recommended to tell if one string is '' instead of a=='' as this may cause a runtime error.
  !============================================
  print*
  print*, 'tell string status...'
  print*, empty%isEmpty()
  empty = ''
  print*, empty%isEmpty()
  empty = ' '
  print*, empty%isEmpty()

  !============================================
  ! 测试：重载运算符，
  ! test:reloading operator
  !============================================
  print*
  print*, 'reloading operator...'
  
  !和字符串比较的区别, Fortran中，字符串比较时，默认右端空格不影响比较结果
  print*, '   pwr'=='pwr', 'pwr   '=='pwr'
  strA =  '   pwr'
  strB =  'pwr'
  print*, strA == strB, strA /= strB
  print*, '   pwr' == strB, strA == 'pwr'
  
  !赋值和加法
  reactor = 'bwr'
  pellet = 'uo2'
  cladding = 'zr4'
  print*, '!' + reactor + ' = ' // cladding + ' + ' + pellet
  
  !String数组赋值
  chars = 'pwr'
  reactor = 'bwr'
  reactors = ['pwr','bwr','vver','wwer','candu']
  charsArr = ['pwr','bwr','vver','wwer','candu']
  !和String数组；
  print*, 'pwr'.in.reactors,      'pwr'.notIn.reactors
  print*, 'PWR'.in.reactors,      'PWR'.notIn.reactors
  print*, chars.in.charsArr,      chars.notIn.charsArr
  print*, reactor.in.reactors, reactor.notIn.reactors
  print*, reactor.in.charsArr, reactor.notIn.charsArr
  
  !============================================
  ! 测试：依规则判断字符串是否相等
  ! test: tell if string equeal strIn. by regulation
  !============================================
  print*
  print*, 'tell if string equeal strIn. by regulation...'
  strA =  '  pwr'
  strB = '  PWR  '
  print*, .false.,  strA%eq(strB,re='i'),  strA%eq('  PWR  ',re='i'),  strA%ne(strB,re='i')
  print*, .true.,   strA%eq(strB,re='ir'), strA%eq('  PWR  ',re='ir'), strA%ne(strB,re='ir')
  print*, .true.,   strA%eq(strB,re='irl'),strA%eq('  PWR  ',re='irl'),strA%ne(strB,re='irl')
  print*, .false.,  strA%eq(strB,re='rl'), strA%eq('  PWR  ',re='rl'), strA%ne(strB,re='rl')

  !============================================
  ! 测试：大小写
  ! test: uppercase and lowercase
  !============================================
  print*
  print*, 'uppercase and lowercase...'
  a = 'x.y.SHANG@hotmail.com'
  print*, a%uppercase()
  print*, a%lowercase()
  print*, a

  !============================================
  ! 测试：字符串对齐
  ! test: string alignment
  !============================================
  print*
  print*, 'string alignment...'
  a = 'chars'
  print*, '123456789_123456789_123456789'
  print*, a%adjust(20,-1)
  print*, a%adjust(20,0)
  print*, a%adjust(20,1)
  print*, a

  !============================================
  ! 测试：分割字符串
  ! 说明：默认分割符为空格，也支持同时指定多个字符为分割符号
  ! test: split string
  ! note: the default seperator is blank, several characters as seperator is also support
  !============================================
  print*
  print*, 'split string...'
  a = 'x.y.SHANG@hotmail.com'
  do i=1, a%split(strs,'@.')
    print*, strs(i)
  enddo
  
  !============================================
  ! 测试：数据类型判断
  ! test: tell the type of the string
  ! 0-非数值的字符串(non-value string)
  ! 1-整数(integer)
  ! 2-小数(fixed point real)
  ! 3-指数类型实数(exponent type real)
  ! 4-双精度实数指数形式(exponent type double)
  ! 5-逻辑类型(logical data)
  ! 6-变量名(variable name)
  !============================================
  print*
  print*, 'tell the type of the string...'
  a = '@cc 11 s1 0.01 .3 3d0 3e0 t F'
  do i=1, a%split(strs)
    print*, strs(i), strs(i)%typeof()
  enddo
  
  !============================================
  ! 测试：字符串拼接
  ! test: concatenate string Array with seperator
  !============================================
  print*
  print*, 'concatenate string Array with seperator...'
  a = ','
  print*, a%join(reactors)
  print*, a
  
  !============================================
  ! 测试：字符串替换
  ! test: replace sub. string old with sub. string new
  !============================================
  print*
  print*, 'replace sub. string old with sub. string new...'
  a = 'old---oldold---old'
  print*, a%replace('old','new')
  print*, a%replace('old','oldold')
  print*, a
  !============================================
  ! 测试：将tab键转换为对应的空格
  ! test: change tab to blanks
  !============================================
  print*
  print*, 'change tab to blanks...'
  open(unit=11,file='./txt.txt',status='old')
  read(11,'(a)') chars
  a = trim(chars)
  print*, '123456789_123456789_123456789'
  print*, a%tab2blanks(20)
  print*, a
  
  !============================================
  ! 测试：将字符串逆序排列
  ! test: reverse string
  !============================================
  print*
  print*, 'reverse string...'
  a = '123456789  '
  print*, a%reverse()
  print*, a
  
  !============================================
  ! 测试：字符串转数值
  ! test: read value from string
  !============================================
  print*
  print*, 'reverse string...'
  a = '1 s1 1d0 t .1'
  !iErr = 1
  do i=1,a%split(strs)
    selectcase(strs(i)%typeof())
    case(1)
      iErr = strs(i)%toNum(vInt)
    case(2:4)
      iErr = strs(i)%toNum(vReal)
    case(5)
      iErr = strs(i)%toNum(vLog)
    case default
      iErr = 1
    endselect
    print*, iErr
  enddo
  
  end program
