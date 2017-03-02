# SETTING UP POSTFIX -- MAC

SOURCE: <http://www.developerfiles.com/how-to-send-emails-from-localhost-mac-os-x-el-capitan/>

How to send emails from localhost (MAC OS X El Capitan)

## Step 1. Edit Postfix config file
Open a terminal and edit the file main.cf

```{sh}
sudo vi /etc/postfix/main.cf
```

First check Postfix is configured correctly, look for the following lines (They are probably separated):

```{sh}
mydomain_fallback = localhost
mail_owner = _postfix
setgid_group = _postdrop
Now add the following lines at the very end of the file:

# Gmail SMTP
relayhost=smtp.gmail.com:587

# Enable SASL authentication in the Postfix SMTP client.
smtp_sasl_auth_enable=yes
smtp_sasl_password_maps=hash:/etc/postfix/sasl_passwd
smtp_sasl_security_options=noanonymous
smtp_sasl_mechanism_filter=plain

# Enable Transport Layer Security (TLS), i.e. SSL.
smtp_use_tls=yes
smtp_tls_security_level=encrypt
tls_random_source=dev:/dev/urandom
```

This is telling Postfix to use a GMAIL SMTP server with Simple Authentication and Security Layer (SASL). Which will be stored in the path “/etc/postfix/sasl_passwd“. You can use any other SMTP provider (Hotmail, Yahoo, ETC...). You only need to know the SMTP host and port. For example for hotmail you should replace the relayhost for the following:

```
#Hotmail SMTP
relayhost=smtp.live.com:587
```
```
#Yahoo SMTP
relayhost=smtp.mail.yahoo.com:465
```

## Step 2. Create the sasl_passwd file
We need to create the sasl_passwd file with the SMTP credentials

```
sudo vi /etc/postfix/sasl_passwd
```
Write the following content and save:

```
smtp.gmail.com:587 your_email@gmail.com:your_password
```
Create the Postfix lookup table from the sasl_passwd file.

```
sudo postmap /etc/postfix/sasl_passwd
```
This will create the file sasl_passwd.db

## Step 3. Restart Postfix
To apply all new changes we have to restart Postfix:

```
sudo postfix reload
```

## Step 4. Turn on less secure apps (Only Gmail)
In Gmail we must switch on the option “Access for less secure apps“, otherwise we will get the error: 
SASL authentication failed

## Step 5. Test it!
Let’s send a mail to our own account to be sure everything is working fine:

```
date | mail -s testing your_email@gmail.com
```
You can check the mail queue and the posible delivery errors using “mailq“

```
mailq
```
Use the postfix logs to ensure everything is working as expected:

```
tail -f /var/log/mail.log
```
Other useful commands
To clear the mail queue:
```
sudo postsuper -d ALL
```

# Change From name

```
printf "From: Sender Name <sender@domain.com>\nTo: recipient@domain.com\nSubject: Subject\n\nBody\n" | /usr/sbin/sendmail -F "Sender Name" -f "sender@domain.com" "flo.debarre@gmail.com"
```

# SETTING UP POSTFIX -- UBUNTU
Source: <https://easyengine.io/tutorials/linux/ubuntu-postfix-gmail-smtp/>


