.class public for1
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
putstatic for1/I S
LINE_10:
getstatic for1/I S
getstatic java/lang/System/out Ljava/io/PrintStream;
swap
invokevirtual java/io/PrintStream/println(I)V
getstatic for1/I S
sipush 1
iadd
putstatic for1/I S
getstatic for1/I S
sipush 5
if_icmple LINE_10
   return
.end method
