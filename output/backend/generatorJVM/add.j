.class public add
.super java/lang/Object

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
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "5+2="
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
sipush 5
sipush 2
iadd
getstatic java/lang/System/out Ljava/io/PrintStream;
swap
invokevirtual java/io/PrintStream/println(I)V
   return
.end method
