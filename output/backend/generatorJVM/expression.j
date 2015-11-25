.class public expression
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
ldc "-3+4*(5+6)*7+8-9="
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
sipush 3
ineg
sipush 4
sipush 5
sipush 6
iadd
imul
sipush 7
imul
iadd
sipush 8
iadd
sipush 9
isub
getstatic java/lang/System/out Ljava/io/PrintStream;
swap
invokevirtual java/io/PrintStream/println(I)V
   return
.end method
