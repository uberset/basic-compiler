.class public array
.super java/lang/Object
.field static A [S

.method static <clinit>()V
   .limit stack  1
   .limit locals 0
   sipush    32464
   newarray  short
   putstatic array/A [S
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
ineg
getstatic array/A [S
swap
sipush 32463
swap
sastore
invokestatic  java/lang/System/console()Ljava/io/Console;
invokevirtual java/io/Console/readLine()Ljava/lang/String;
invokestatic  java/lang/Integer/parseInt(Ljava/lang/String;)I
getstatic array/A [S
swap
sipush 0
swap
sastore
sipush 32463
getstatic array/A [S
swap
saload
getstatic java/lang/System/out Ljava/io/PrintStream;
swap
invokevirtual java/io/PrintStream/println(I)V
sipush 0
getstatic array/A [S
swap
saload
getstatic java/lang/System/out Ljava/io/PrintStream;
swap
invokevirtual java/io/PrintStream/println(I)V
sipush 1
getstatic array/A [S
swap
saload
getstatic java/lang/System/out Ljava/io/PrintStream;
swap
invokevirtual java/io/PrintStream/println(I)V
   return
.end method
