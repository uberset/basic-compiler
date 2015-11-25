.class public hello
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
ldc "Hello World!"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
   return
.end method
