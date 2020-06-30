#!/bin/bash
export IFS=$'\n';
arg=$1;
echo $arg;
if [ $arg == "post" ];
then
    notmuch new;
    notmuch tag +important folder:\[Gmail\].Important;
    notmuch tag +starred folder:\[Gmail\].Starred;
    notmuch tag +drafts folder:\[Gmail\].Drafts;
    notmuch tag +sent folder:sent;
    notmuch tag +trash  folder:\[Gmail\].Trash;
    notmuch tag +spam folder:\[Gmail\].Spam;
    notmuch tag +inbox folder:INBOX;
    notmuch tag +archived not tag:drafts and not tag:sent and not tag:trash and not tag:spam and not tag:inbox;
elif [ $arg == "pre" ];
then
    
    for i in $(notmuch search  --output=files tag:deleted);
    do
	mv -v "$i" /home/arya/Documents/Mail/Gmail/\[Gmail\].Trash/cur;	
    done
    for i in $(notmuch search  --output=files tag:archive);
    do
	mv -v "$i" /home/arya/Documents/Mail/Gmail/\[Gmail\].All\ Mail/cur;
    done

    for i in $(notmuch search  --output=files tag:spam);
    do
				mv -v "$i" /home/arya/Documents/Mail/Gmail/\[Gmail\].Spam/cur;
    done

    notmuch new;
    notmuch tag -inbox tag:deleted;
    notmuch tag -spam tag:deleted;
    notmuch tag -sent tag:deleted;
    notmuch tag -drafts tag:deleted;
    notmuch tag -deleted tag:deleted;
    notmuch tag -inbox tag:archive;
    notmuch tag -spam tag:archive;
    notmuch tag -sent tag:archive;
    notmuch tag -drafts tag:archive;
    notmuch tag -deleted tag:archive;
    notmuch tag -archive tag:archive;

		notmuch tag -inbox tag:spam;
    notmuch tag -sent tag:spam;
    notmuch tag -deleted tag:spam;
    notmuch tag -archive tag:spam;

else
    echo "Nothing";
fi

