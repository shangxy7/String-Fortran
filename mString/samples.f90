
  program samples
  !1.���ã�citing
  use mString
  implicit none

  !2.������creating
  type(String)::a,empty,strA,strB
  type(string)::cladding,pellet,reactor,reactors(5),strs(100)
  character(len=20)::chars, charsArr(5)
  integer::i, iErr, vInt
  real(8)::vReal
  logical::vLog

  !3.��ֵ��assignment
  a = 'Happy new year...'

  !============================================
  ! ���ԣ��ַ���״̬�ж�
  ! ע�⣺ǿ�ҽ���ʹ��a%isEmpty()�������ж�String�����Ƿ�Ϊ�գ�������ʹ��a == '', �������п��ܴ�������ʱ����
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
  ! ���ԣ������������
  ! test:reloading operator
  !============================================
  print*
  print*, 'reloading operator...'
  
  !���ַ����Ƚϵ�����, Fortran�У��ַ����Ƚ�ʱ��Ĭ���Ҷ˿ո�Ӱ��ȽϽ��
  print*, '   pwr'=='pwr', 'pwr   '=='pwr'
  strA =  '   pwr'
  strB =  'pwr'
  print*, strA == strB, strA /= strB
  print*, '   pwr' == strB, strA == 'pwr'
  
  !��ֵ�ͼӷ�
  reactor = 'bwr'
  pellet = 'uo2'
  cladding = 'zr4'
  print*, '!' + reactor + ' = ' // cladding + ' + ' + pellet
  
  !String���鸳ֵ
  chars = 'pwr'
  reactor = 'bwr'
  reactors = ['pwr','bwr','vver','wwer','candu']
  charsArr = ['pwr','bwr','vver','wwer','candu']
  !��String���飻
  print*, 'pwr'.in.reactors,      'pwr'.notIn.reactors
  print*, 'PWR'.in.reactors,      'PWR'.notIn.reactors
  print*, chars.in.charsArr,      chars.notIn.charsArr
  print*, reactor.in.reactors, reactor.notIn.reactors
  print*, reactor.in.charsArr, reactor.notIn.charsArr
  
  !============================================
  ! ���ԣ��������ж��ַ����Ƿ����
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
  ! ���ԣ���Сд
  ! test: uppercase and lowercase
  !============================================
  print*
  print*, 'uppercase and lowercase...'
  a = 'x.y.SHANG@hotmail.com'
  print*, a%uppercase()
  print*, a%lowercase()
  print*, a

  !============================================
  ! ���ԣ��ַ�������
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
  ! ���ԣ��ָ��ַ���
  ! ˵����Ĭ�Ϸָ��Ϊ�ո�Ҳ֧��ͬʱָ������ַ�Ϊ�ָ����
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
  ! ���ԣ����������ж�
  ! test: tell the type of the string
  ! 0-����ֵ���ַ���(non-value string)
  ! 1-����(integer)
  ! 2-С��(fixed point real)
  ! 3-ָ������ʵ��(exponent type real)
  ! 4-˫����ʵ��ָ����ʽ(exponent type double)
  ! 5-�߼�����(logical data)
  ! 6-������(variable name)
  !============================================
  print*
  print*, 'tell the type of the string...'
  a = '@cc 11 s1 0.01 .3 3d0 3e0 t F'
  do i=1, a%split(strs)
    print*, strs(i), strs(i)%typeof()
  enddo
  
  !============================================
  ! ���ԣ��ַ���ƴ��
  ! test: concatenate string Array with seperator
  !============================================
  print*
  print*, 'concatenate string Array with seperator...'
  a = ','
  print*, a%join(reactors)
  print*, a
  
  !============================================
  ! ���ԣ��ַ����滻
  ! test: replace sub. string old with sub. string new
  !============================================
  print*
  print*, 'replace sub. string old with sub. string new...'
  a = 'old---oldold---old'
  print*, a%replace('old','new')
  print*, a%replace('old','oldold')
  print*, a
  !============================================
  ! ���ԣ���tab��ת��Ϊ��Ӧ�Ŀո�
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
  ! ���ԣ����ַ�����������
  ! test: reverse string
  !============================================
  print*
  print*, 'reverse string...'
  a = '123456789  '
  print*, a%reverse()
  print*, a
  
  !============================================
  ! ���ԣ��ַ���ת��ֵ
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
