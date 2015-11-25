.class public input
.super java/lang/Object
.field static X S

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
invokestatic  java/lang/System/console()Ljava/io/Console;
invokevirtual java/io/Console/readLine()Ljava/lang/String;
invokestatic  java/lang/Integer/parseInt(Ljava/lang/String;)I
putstatic input/X S
getstatic input/X S
sipush 1
iadd
getstatic java/lang/System/out Ljava/io/PrintStream;
swap
invokevirtual java/io/PrintStream/println(I)V
   return
.end method
