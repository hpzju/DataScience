---
id: Howto-SSLCertificate
title: Howto Setup SSL Certificate with Nginx for Websites
sidebar_label: Setup SSL Certificate with Nginx
---

## Backround

- [PKI](https://en.wikipedia.org/wiki/Public_key_infrastructure)
- [DNS](https://en.wikipedia.org/wiki/Domain_Name_System) and [Domain Name](https://en.wikipedia.org/wiki/Domain_name)
- [Nginx](https://nginx.org/en/) is a leading HTTP and reverse proxy server software.
- SSL Certificate helps to secure website by encrypting communication between client side and server side.

---

## Prerequisite and Scenario

- Website is up and running with Nginx.
- Self-signed SSL certificate scenario
- Free CA issued SSL certificate scenario.

---

## Procedure

### Step 1 – Creating SSL certificate

#### 1.1 Self-signed SSL certificate

- login to your web server shell
- generate public key and private key
  - `sudo openssl req -x509 -nodes -days 365 -newkey rsa:2048 -keyout /etc/ssl/private/nginx-selfsigned.key -out /etc/ssl/certs/nginx-selfsigned.crt`
- strengthen Forward Secracy
  - `sudo openssl dhparam -out /etc/nginx/dhparam.pem 4096`

#### 1.2 Free CA issued SSL certificate

- go to [sslforfree website](https://www.sslforfree.com/), obtain publick key and private key.
  - follow this [video on Youtube](https://www.youtube.com/watch?v=wmb0sSzFix0) step by step
  - Notice: this step requires to verify Domain Name Owner, you need have full access to you domain name records to finish it.
  - Notice: the certificate has 3 month validation period, you need re-issue new certificate every 3 month.
- Rename obtained files:

  - rename public key `certificate.crt` to `nginx-selfsigned.crt`
  - rename private key `private.key` to `nginx-selfsigned.key`

- Move `nginx-selfsigned.crt` and `nginx-selfsigned.key` to web server
  - `/etc/ssl/certs/nginx-selfsigned.crt`
  - `/etc/ssl/private/nginx-selfsigned.key`
  - Notice: private key must change to `chmod 600` if neccessary, the best way to avoid this is to generate self-signed public and private keys, then copy the text content of free CA issued keys into self-signed keys accordingly.

### Step 2 - Configuring Nginx to use SSL

#### 2.1 Create Nginx config snippets for certificate

- `sudo vim /etc/nginx/snippets/self-signed.conf`

        ssl_certificate /etc/ssl/certs/nginx-selfsigned.crt;

        ssl_certificate_key /etc/ssl/private/nginx-selfsigned.key;

#### 2.2 Create Nginx config snippets for forward secracy

- `sudo vim /etc/nginx/snippets/ssl-params.conf`

        ssl_protocols TLSv1.2;
        ssl_prefer_server_ciphers on;
        ssl_dhparam /etc/nginx/dhparam.pem;
        ssl_ciphers ECDHE-RSA-AES256-GCM-SHA512:DHE-RSA-AES256-GCM-SHA512:ECDHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-SHA384;
        ssl_ecdh_curve secp384r1; # Requires nginx >= 1.1.0
        ssl_session_timeout  10m;
        ssl_session_cache shared:SSL:10m;
        ssl_session_tickets off; # Requires nginx >= 1.5.9
        ssl_stapling on; # Requires nginx >= 1.3.7
        ssl_stapling_verify on; # Requires nginx => 1.3.7
        resolver 8.8.8.8 8.8.4.4 valid=300s;
        resolver_timeout 5s;
        # Disable strict transport security for now. You can uncomment the following
        # line if you understand the implications.
        # add_header Strict-Transport-Security "max-age=63072000; includeSubDomains; preload";
        add_header X-Frame-Options DENY;
        add_header X-Content-Type-Options nosniff;
        add_header X-XSS-Protection "1; mode=block";

---

#### 2.3 Config Nginx server section to enable SSL certificate

- bakcup your website config file
  - `sudo cp /etc/nginx/sites-available/example.com /ect/nginx/sites-available/example.com.backup`
- add new server section for redirecting http to https in `example.com` config file

        #example.com server http redirect to https:
        server {
            listen 80;
            listen [::]:80;

            server_name www.example.com;
            return 302 https://$server_name$request_uri;
        }

  - Notice: the above config assumes seperate site server config in Nginx, if you want redirect all http, add redirect instructions to default_server section
  - Notice see [difference of return 301 and 302](https://stackoverflow.com/questions/1393280/http-redirect-301-permanent-vs-302-temporary)

- edit existing server section, add https listen, SSL certificate snnipet and Foward Secracy snnipet in `example.com` config file:

        #example.com server:
        server {
            listen 443 ssl http2;
            listen [::]:443 ssl http2;
            ...
            ...
            server_name www.example.com;
            include snippets/self-signed.conf;
            include snippets/ssl-params.conf;
            ...
            ...
        }

#### 2.4 Adjust firewall rule in web server

- [ufw documentation](https://linuxize.com/post/how-to-setup-a-firewall-with-ufw-on-ubuntu-18-04/)
- `sudo ufw enable`
- `sudo ufw app list`
- `sudo ufw allow 'Nginx Full'`
- `sudo ufw delete allow 'Nginx HTTP'`
- `sudo ufw status`

### Step 3 - Restart Nginx service

- `sudo nginx -t`
- `sudo systemctl restart nginx`
- `sudo systemctl status nginx`

### Step 4 - Check your http redirection and https services

- https service:
  - Self-signed SSL Certificate cases:
    - browser warning:
      ![Alt](https://assets.digitalocean.com/articles/nginx_ssl_1604/self_signed_warning.png "Self-signed SSL Certificate Warning")
    - proceed to
      ![Alt](https://assets.digitalocean.com/articles/nginx_ssl_1604/warning_override.png "proceed to unsafe")
- http redirection service:
  - Input `http://www.example.com` to web browser, check website returns `https://www.example.com` connection.

## Result

- secured https service for your website.

---

## Pitfalls and Cautions

- SSL for Free's SSL certificate expired every 3 month, you need reapply new one every 3 month.
- Apply free SSL certificate requires Domain Name ownership verification.
- Self-signed certificate is not a good option for commercial use case.

---

## Reference Resources

- [SSL for Free](https://www.sslforfree.com/)
- [DigitalOcean Tutorial](https://www.digitalocean.com/community/tutorials/how-to-create-a-self-signed-ssl-certificate-for-nginx-in-ubuntu-18-04)
