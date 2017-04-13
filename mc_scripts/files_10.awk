$0 ~ /\${file1}/ { for (i=1;i<=10;i++) {
	newline = gensub(/\${file1}/,i+(num-1)*10,"g");
	print newline;
	nmatch = match(newline,/(\/mss\/hallb\/.*\.slcio).*dest/,ary);
#	nmatch = match(newline,/(\/work\/.*\.lhe.gz).*dest/,ary);
	nmatch = match(newline,/Input.src=\".*:(.*)\".*dest/,ary);
#	tmp=gensub(/\${apmass}/,ap,"g",ary[1]);
	file=gensub(/\${ebeam}/,ebeam,"g",ary[1]);
#	print file;
	if( system( "[ -f " file " ] " )  == 0 ){
	    print newline;   
	}
    }
}
$0 !~ /\${file1}/ { print $0 }

