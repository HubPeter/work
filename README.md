source file:
	compress.lisp:
		加密的同时达到压缩的效果
	en.lisp:
		使用异或方式对8位bmp图像进行混沌加密

image file:
	en.lisp：
		plain.bmp:	原始位图
		ciper.bmp:	加密后的图像
		de.bmp：	解密后的图像
	compress.lisp:
		p_*.jpg:	原始图像	e.g. p_girl.jpg
		c_*.jpg:	加密后的图像

mail:	tecnodechina@gmail.com

安装
	光盘中已有压缩包，相关问题可以访问下面的网址
	lispbox: 
		http://common-lisp.net/project/lispbox/
	

运行程序
	;; 编译源文件
	CL>(load "compress.lisp")
	;; 运行函数，可以用该方式运行任何函数
	;;   无参
	(main)
	;;   带参
	(encrypt "plain.bmp" "ciper.bmp")
