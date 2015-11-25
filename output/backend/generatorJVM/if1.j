.class public if1
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
   .limit stack 10000
sipush 1
putstatic if1/I S
LINE_10:
getstatic if1/I S
getstatic if1/I S
imul
getstatic java/lang/System/out Ljava/io/PrintStream;
swap
invokevirtual java/io/PrintStream/println(I)V
getstatic if1/I S
sipush 1
iadd
putstatic if1/I S
getstatic if1/I S
sipush 5
if_icmple LINE_10
   return
.end method
