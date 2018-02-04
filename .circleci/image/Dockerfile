FROM golang:1.8.0
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash -
RUN apt-get update && apt-get install -y nodejs
RUN npm install -g elm
RUN npm install -g docker
RUN npm install -g elm-format
# The purpose of this libsysconfcpus is supposedly to make elm compile faster.
RUN git clone https://github.com/obmarg/libsysconfcpus.git;
RUN cd libsysconfcpus && ./configure && make && make install && cd ../

