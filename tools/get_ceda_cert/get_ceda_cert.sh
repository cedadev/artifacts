#!/bin/bash
#
# Client script for web service interface to SLCS get-trustroots based on 
# curl and base64 commands.  Get trust roots retrieves the CA certificate 
# issuer(s) of the SLCS's SSL certificate
#
# @author P J Kershaw 07/06/2010
# @copyright: (C) 2010 STFC
# @license: BSD - See top-level LICENCE file for licence details
# 
# Simplified by Sam Pepler 2021
# $Id$

# set parameters for making the certificate
uri_trustroots=https://slcs.ceda.ac.uk/onlineca/trustroots/ 
cadir=/tmp/ceda_trustroots 
uri_cert=https://slcs.ceda.ac.uk/onlineca/certificate/
outfilepath=$PWD/ceda_creds.pem
username=$1
if [  -z "$username" ]; then
    echo "Need a CEDA username as an argument: $0 <username>"
    exit 1
fi

# Read password
if [ $stdin_pass ]; then
    read password;
else
    stty -echo
    read -p "Enter Short-Lived Credential phrase: " password; echo
    stty echo
fi

# Set-up destination trust root directory
if [ ! -d $cadir ]; then
    mkdir -p $cadir
fi

# Set peer authentication based on bootstrap command line setting
echo Bootstrapping Short-Lived Credential Service root of trust.

# Make a temporary file for error output
error_output_filepath="/tmp/$UID-$RANDOM.csr"

# Post request to Short-Lived Credential Service
response=$(wget $uri_trustroots  --secure-protocol TLSv1 --no-check-certificate -t 1 -O - 2> $error_output_filepath)

# Extract error output and clean up
error_output=$(cat $error_output_filepath)
rm -f $error_output_filepath

# Pull out the response code from the error output
wget_statcode_line="awaiting response..."
responsecode=$(echo "$error_output"|grep "$wget_statcode_line"|awk '{print $6}')
if [ "$responsecode" != "200" ]; then
    echo "Get trust roots failed"
    echo "$error_output" >&2
    exit 1
fi

# Process response
entries=$(echo $response|awk '{print $0}')
for i in $entries; do
    filename=${i%%=*}
    filecontent="$(echo ${i#*=}|awk '{for(i=1;i<length;i+=65) print substr($0,i,65)}'|openssl enc -d -base64)"
    echo "$filecontent" > $cadir/$filename
done

# Make a temporary file location for the certificate request and key
keyfilepath="/tmp/$UID-$RANDOM.key"
certreqfilepath="/tmp/$UID-$RANDOM.csr"

# Generate key pair and request.  The key file is written to the 'key' var
openssl req -new -newkey rsa:2048 -nodes -keyout $keyfilepath -subj //\CN=dummy -out $certreqfilepath 2> /dev/null
key=$(cat $keyfilepath)

# URL Encode certificate request - allow for '+' symbol in the base64 charset -
# needs to be hex equivalent
# Post request to Short-Lived Credential service passing username/password for HTTP Basic
# auth based authentication.
encoded_certreq=$(cat $certreqfilepath|sed s/+/%2B/g)

# Clean up certificate request and key temporary files
rm -f $keyfilepath
rm -f $certreqfilepath

response=$(wget --secure-protocol TLSv1 --ca-directory=$cadir \
--http-user=$username --http-password=$password \
--post-data "certificate_request=$encoded_certreq" \
-t 1 $uri_cert -O - 2>&1)

# tidy temp cadir
rm -rf $cadir

# Pull out the response code from the output
wget_statcode_line="awaiting response..."
responsecode=$(echo "$response"|grep "$wget_statcode_line"|awk '{print $6}'|tail -1)
if [ "$responsecode" != "200" ]; then
    echo "Get certificate call failed."
    echo "$response" >&2
    exit 1
fi

# Extract the certificate(s) - there may be additional certificates forming
# part of a chain of trust back to a root CA.
cert=$(echo "$response" | sed -n '/BEGIN CERTIFICATE/,/END CERTIFICATE/p')

# Simple sanity check on extracted cert
if [[ $cert != -----BEGIN\ CERTIFICATE-----* ]]; then
    echo "Expecting certificate in response; got:"
    echo "$cert" >&2
    exit 1
fi

# Output certificate
echo "$cert" > $outfilepath

# Add key
echo "$key" >> $outfilepath

echo "Made a certificate $outfilepath"
echo 
echo "You can use it like this:"
echo $ wget --certificate=ceda_creds.pem http://dap.ceda.ac.uk/badc/acsoe/data/00README
echo $ curl --cert $PWD/creds.pem ceda_creds.pem http://dap.ceda.ac.uk/badc/acsoe/data/00README
