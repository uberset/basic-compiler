.class public fornext
.super java/lang/Object
.field static I S

.method static <clinit>()V
   .limit stack  1
   .limit locals 0
   return
.end method

.method public <init>()V
   aload_0
   invokenonvirtual java/lang/Object/<init>()V
   return
.end method
.method public static main([Ljava/lang/String;)V
   .limit locals 40
   .limit stack 10000
sipush 1
putstatic fornext/I S
LINE_10:
getstatic fornext/I S
getstatic fornext/I S
imul
getstatic java/lang/System/out Ljava/io/PrintStream;
swap
invokevirtual java/io/PrintStream/println(I)V
getstatic fornext/I S
sipush 2
iadd
putstatic fornext/I S
getstatic fornext/I S
sipush 9
if_icmple LINE_10
   return
.end method
