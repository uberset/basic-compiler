.class public gosubre
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
jsr LINE_100
jsr LINE_100
jsr LINE_100
goto LINE_999
LINE_100:
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "Hello!"
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
ret
LINE_999:
   return
.end method
