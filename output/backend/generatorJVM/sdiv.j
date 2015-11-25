.class public sdiv
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
sipush 1
ineg
putstatic sdiv/X S
getstatic java/lang/System/out Ljava/io/PrintStream;
ldc "-1/-1="
invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
getstatic sdiv/X S
getstatic sdiv/X S
idiv
getstatic java/lang/System/out Ljava/io/PrintStream;
swap
invokevirtual java/io/PrintStream/println(I)V
   return
.end method
