# Pceudo

Захотелось мне более читаемого и интуитивного ассемблера.
Сейчас более менее работает версия под stm8s (наполняю библиотеку макросов) собственно транслятор уже знает и stm8l и avr - но под них нет пока никаких заголовков - вопрос времени.

Начнем с примера: вот так выглядит примитивный "блинк" на псевдо ассемблерами с макросами:

>include stm8s207x8
>
>include system
>
>include gpio
>
>define led	5
>
>interrupt reset@0x8000: (f32) {0x82000000+start}
>
>delay: x = 65000; dloop: x--; if z=0 dloop; ret
>
>start: Stack.init(ramend); Gpiod.output(led); Gpiod.pushup(led)
>
>loop:  call delay; Gpiod.invert(led); go near loop
>

То же самое без всяких там макросов:

>include stm8s207x8
>
>interrupt reset@0x8000: (f32) {0x82000000+start}
>
>delay: x = 65000; dloop: x--; if z=0 dloop; ret
>
>start: x = ramend; sp = x; gpiod ddr#5 = 1; gpiod acr1#5 = 1
>
>loop: call delay; !gpiod odr#5; go near loop
>

Генерится нативный код (вырезка, там сверху еще преамбула):

>flash	segment byte at: 8000	'flash'
>
>interruptreset:
>
>	dc.l	{$82000000+start}
>
>delay: ldw x,#65000
>
>dloop:	decw x
>
>	jrne dloop
>
>	ret
>
>start:	ldw x,#$17ff
>
>	ldw sp,x
>
>	bset gpiodddr,#5
>
>	bset gpiodcr1,#5
>
>loop:	call delay
>
>	bcpl gpiododr,#5
>
>	jra loop

В этом языке псевдо-ассемблера есть очень мало команд (36 против 81 у stm8), однако каждая команда превращается ровно в одну ассемблерную инструкцию. Нельзя в одной команде сочетать несколько действий - это ассемблер! Команды разделяются символами ";" или "\n". Можно писать несколько команд в одной строке через ';'. После метки ';'ставить не нужно, там есть ':' и этого достаточно.
Нельзя переносить полкоманды на другую строку (например if на одной строке а его метка на другой). Необходимо конечно помнить и об ограничениях в режимах адресации команд нативного ассемблера - они само собой присутствуют и транслятор никак от этого не спасает.

Операции
--------------------
1. КОМЕНТАРИЙ - // текст, можно ставить как в начале так и после любой команды кроме 'define' препроцессора
2. ПРИСВАИВАНИЕ - **X = Y**	( под присваиванием подразумевается и обнуление, и установка/сброс флага и т.п.)
3. СЛОЖЕНИЕ - **X += Y**
4. ВЫЧИТАНИЕ - **X -= Y**
5. УМНОЖЕНИЕ - **X *= Y**	( в версии stm8 реально доступны только x *= a или y *= a)
6. ДЕЛЕНИЕ - **X /= Y**		( в версии stm8 реально доступны только x/=a, y/=a или x/=y - последний меняет и x и y - в y возвращает остаток от деления )
7. ИНВЕРСИЯ ЗНАКА - **-X**
8. ПРОВЕРКА ФЛАГОВ - **?X**
9. СРАВНЕНИЕ **X ? Y**          ( в версии stm8 реально доступны только a?... x?... y?...)
10. ИНКРЕМЕНТ - **X++** или **++X** (два варианта полностью эквивалентны)
11. ДЕКРЕМЕНТ - **X--** или **--X** (два варианта полностью эквивалентны)
12. ЛОГИЧЕСКОЕ AND - **X &= Y**
13. ЛОГИЧЕСКОЕ OR - **X |= Y**
14. ЛОГИЧЕСКОЕ XOR - **X ^= Y**
15. ЛОГИЧЕСКОЕ NOT - **!X**
16. ЛОГИЧЕСКОЕ СРАВНЕНИЕ - **X &? Y** ( в версии stm8 реально доступны только a&?)
17. КОЛЬЦЕВОЙ СДВИГ ЧЕРЕЗ С - **c<X<c**
18. КОЛЬЦЕВОЙ СДВИГ ЧЕРЕЗ С - **c>X>c**
19. СДВИГ УМНОЖЕНИЕ НА 2 - **c<X<0**
20. СДВИГ ЗНАКОВОЕ ДЕЛЕНИЕ НА 2 - **s>X>c**
21. СДВИГ БЕЗЗНАКОВОЕ ДЕЛЕНИЕ НА 2 - **0>X>c**
22. КОЛЬЦЕВОЙ СДВИГ ЧЕРЕЗ A - **a<X<a**		( X только x или y)
23. КОЛЬЦЕВОЙ СДВИГ ЧЕРЕЗ A - **a>X>a**		( X только x или y)
24. ВЫЗОВ ФУНКЦИИ CALL - **call near X** или **call X** или **call far X**
25. ВОЗВРАТ ИЗ ФУНКЦИИ - **ret** или **ret far**
26. ВОЗВРАТ ИЗ ПРЕРЫВАНИЯ - **iret**
27. ПЕРЕХОД БЕЗУСЛОВНЫЙ - **go near X** или **go X** или **go far X**
28. ПЕРЕХОД УСЛОВНЫЙ - **if condition X**
29. ПРОПУСК УСЛОВНЫЙ - **skip X#Y=Z**	(в avr)
30. ОСТАНОВ - **halt**
31. ОСТАНОВ С ОЖИДАНИЕМ СОБЫТИЯ - **wait**
32. ОСТАНОВ С ОЖИДАНИЕМ ПРЕРЫВАНИЯ - **wait i**
33. ПРОГРАММНОЕ ПРЕРЫВАНИЕ - **trap**
34. НИЧЕГО НЕ ДЕЛАНИЕ **nop** или **break** 

Комбинированные операции - генерируют несколько команд:

35. ПЕРЕХОД УСЛОВНЫЙ - **if чтото=0 X** или **if чтото<>0 X** где что то - регистр или переменная

Вставка нативных операций процессора:

36. ВСТАВКА НАТИВНОЙ КОМАНДЫ **inline cmd X,Y;** (внутри inline строки только одна строка ассемблера, но в ней можно использовать препроцессор нативного ассемблера это дает дополнительную свободу при составлении своих макросов)

Режимы адресации
----------------------
1. НЕПОСРЕДСТВЕННЫЙ - **123** - число
2. РЕГИСТРОВЫЙ - **r** - имя регистра. Может быть a,cc,x,xl,xh,y,yl,yh,sp результат - значение регистра
3. ОТНОСИТЕЛЬНЫЙ - **label** - относительный адрес (метка при адресации)
4. ПРЯМОЙ - **label** - результат - значение переменной по адресу данной метки
5. ПОРТ ВВ - **<port>** - результат - значение порта (это для avr)
6. ИНДЕКСНЫЙ - **[r]** - имя регистра может быть x,y,sp результат - значение переменной по адресу из регистра
7. ИНДЕКСНЫЙ - **label[r]** - имя регистра может быть x,y,sp результат - значение переменной по адресу label со смещением из r
8. НЕПРЯМОЙ - **[label]** - значение переменной из расположенной по адресу из ячейки памяти расположенной по адресу хранящейся в ячейке памяти
9. НЕПРЯМОЙ - **[label[r]]** - имя регистра может быть x,y,sp результат - значение переменной из расположенной по адресу из ячейки памяти расположенной по адресу label со смещением из r
10. ПОБИТОВЫЙ - **label#bit** - результат - значение бита по адресу данной метки с номером bit
11. ПОБИТОВЫЙ К ПОРТУ ВВ - **<port>#bit** - результат - значение бита по адресу данной метки с номером bit (для avr)

Метки
--------------------

1. МЕТКА С АДРЕСОМ - **label@address:**
2. МЕТКА - **label:**

Данные
-----------------

1. ПРИВЯЗАННЫЕ ПЕРЕМЕННЫЕ - **(u8)**, **(u16)**, **(u32)** - нужны для привязывания имени к какому либо месту, используются например для описания регистров ввода вывода
2. НЕИНИЦИАЛИЗИРОВАННЫЕ ПЕРЕМЕННЫЕ - **(x8)**, **(x16)**, **(x32)** где x = {r,m} r - псевдорегистровая память, m - оперативная память
3. ИНИЦИАЛИЗИРОВАННЫЕ ЗНАЧЕНИЯ - **(x8) {val}**, **(x16) {val}**, **(x32) {val}** где x = {f,e} f - флэш память, e - eeprom
4. ПРИВЯЗАННЫЕ МАССИВЫ - **(u8)[n]**, **(u16)[n]**, **(u32)[n]** где n - размерность массива
5. НЕИНИЦИАЛИЗИРОВАННЫЕ МАССИВЫ - **(x8)[n]**, **(x16)[n]**, **(x32)[n]** где x = {r,m} r - псевдорегистровая память, m - оперативная память, n - размерность массива
6. ИНИЦИАЛИЗИРОВАННЫЕ МАССИВЫ - **(x8)[n] {vals}**, **(x16)[n]{vals}**, **(x32)[n]{vals}**, **(x8)[n]"line"** где x = {f,e} f - флэш память, e - eeprom, vals,line - данные для инициализации массива

Препроцессор
----------------------
1. ПОДКЛЮЧИТЬ ФАЙЛ - **include filename**{.ext}
2. ОПРЕДЕЛИТЬ КОНСТАНТУ - **define const{ value}**
3. ОПРЕДЕЛИТЬ МАКРОС - **define name(parameter1{,parameter2...}) macros_body**

В макросе можно использовать несколько строк:

define a(b) a=b;\   // обязательно завершайте команды в обрезанных строках символом ';'

b++;\
	
c=a;

В макросе можно использовать локальные метки:

define a(b) **@1:** b--; if z=0 @1;

4. УСЛОВНАЯ КОМПИЛЯЦИЯ	- **ifdef const** ... **endif** или **ifndef const** ... **endif**

Препроцессор примитивный, однопроходный и тупо подменяющий строки. Будьте осторожнее. В именах констант и макросов можно использовать английские буквы, цифры и символ точки.

Особенности
----------------

Ассемблер экспериментальный, поэтому хотелось сделать программу-транслятор максимально простой и легко модифициуемой, поскольку
сам окончательный язык в начале не был ясен, а рождался по ходу написания транслятора. За простоту транслятора приходится платить.

При компиляции из всего кода удаляются все пробелы (исключения ниже). Это дает возможность вставлять пробелы для удобочитаемости внутрь переменных и даже чисел, апример 'gpioa odr#0 = 1' и 'if z = 0 loop'. НО убираются абсолютно все пробелы, в том числе из строк инициализации данных ( " тут все пробелы будут удалены")! Решение:

line:	(f8)[3] {"тут"}; **(f8) {32};** (f8)[6] {"строка"}; (а8) {0}

Да, не элегантно, но так ли часто мы используем строки на крохотных контроллерах?
Обещанные **исключения**: нельзя разбавлять пробелами инструкции препроцессора - он работает до компилятора, до "сжатия" пробелов. И не жмутся пробелы при вставке команд нативного ассемблера: inline elpm X; -> 'elpm X'.

Еще одна неприятная неявность заключается в разбиении строк на "команды" по ';'. То есть строка 

(f8)[27]"бедная строка; ее разрежут", 

будет распилена на две непонятных команды 

**(f8)[27]"беднаястрока** и **ееразрежут"**. 

Решение аналогично борьбе с пробелами.

Библиотеки макросов (stm8)
--------------------------
Сначала чуть чуть о stm8 после reset:

* тактировние от внутреннего генератора (hsi) с множителем 8 - частота fmaster = 2 МГц
* тактирование всех периферийных устройств включено
* прерывания глобально запрещены

Итак...

### Системные, файл 'system'

* **Stack.init(param)**		- инициализирует указатель стека в значение 'param'
* **Interrupt.disable**		- общий запрет прерываний
* **Interrupt.enable**		- общее разрешение прерываний 

Прерывния (я обернул столь простые команды - i=0 и i=1 в макрос потому что интуитивно не понятно что 1 это запрет, а 0 разрешение)

### Порты ввода-вывода, файл 'gpio'

Дам имена макросов для Gpioa - для остальных аналогично

* **Gpioa.output(pin)** - настроить ногу pin как выход
* **Gpioa.pushup(pin)** - подключить в pin резистор подтягивающий ногу к питанию
* **Gpioa.float(pin)** - отключить в pin резистор подтягивающий ногу к питанию
* **Gpioa.fast(pin)** - включить в pin режим при котором скорость реакции выше но и потребление выше
* **Gpioa.slow(pin)** - включить в pin режим при котором скорость реакции ниже но и потребление меньше
* **Gpioa.set(pin)** - выставить на ноге pin значение "1"
* **Gpioa.clr(pin)** - выставить на ноге pin значение "0"
* **Gpioa.invert(pin)** - инверсия значения на ноге pin
* **Gpioa.setvalue(value)** - присвоить порту значение

* **Gpioa.input(pin)** - настроить ногу pin как вход
* **Gpioa.interrupt(pin)** - разрешить прерывания по ноге pin
* **Gpioa.disableinterrupt(pin)** - запретить прерывания по ноге pin
* **Gpioa.getvalue** - прочитать значение из порта
* **Gpioa.get(pin)** - прочитать значение на ноге pin

### Тактирование периферийных устройств, файл 'power'

* **Power.on.all** - включить тактирование всех устройств
* **Power.off.all** - выключить тактирование всех устройств
* **Power.on.имяустройства** - включить тактирование устройства с указанным именем
* **Power.off.имяустройства** - выключить тактирование устройства с указанным именем

Допустимые имена устройств: tim1,tim2,tim3,tim4,uart1,uart2,spi,i2c,awu,adc

### Запись в EEPROM и FLASH, файл 'flash'

* **Flash.unprotect** - разрешить запись в область flash
* **Flash.protect** - запретить запись во flash
* **Eeprom.unprotect** - разрешить запись в область eeprom
* **Eeprom.protect** - запретить запись во eeprom

### Последовательные порты простой обмен без буферизации, файлы 'uart1', 'uart2'

Оба uart есть только в 20х серии stm8s в более младших есть либо uart1 либо uart2. Чтобы улучшить переносимость кода, между младшими
контроллерами вместо Uart{X} можно использовать префикс **Serial**. Serial автоматически заменяется на имеющийся в данном контроллере uart.
В контроллерах 20x серии Serial - псевдоним uart с младшим номером из имеющихся. Макросы даю для Uart1, для Uart2 они такие-же.
Это простая небуферизированная передача без прерываний и т.п.

* **Uart1.init(fmaster,speed)** - инициализация uart
* **Uart1.putc()** - послать байт находящийся в регистре 'a'
* **Uart1.puts(s)** - послать 0-терминированную строку лежащую по адресу s 
* **Uart1.getc** - принять байт, положить его в регистр 'a'

Кроме скорости надо дополнительно инициализировать "ноги", а они для разных контроллеров могут быть разными
поэтому это надо сделать отдельной командой:

* **Uart.atD5D6**
* **Uart.atA4A5**

Можно конечно это сделать и используя просто gpio...

Продолжение следует
