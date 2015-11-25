.class public hello2
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
   .limit stack 10000
LINE_10:
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "Hello"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
LINE_20:
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "World!"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
   return
.end method
