FROM scratch

# Add the application binary
ADD bin/server /

# Add compiled static files
ADD web /web

CMD ["/server", "--port=80"]

EXPOSE 80
