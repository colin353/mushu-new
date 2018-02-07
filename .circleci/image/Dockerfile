FROM golang:1.8.0

# Add external repository keys.
RUN curl -sL https://deb.nodesource.com/setup_6.x | bash -
RUN curl -fsSL https://download.docker.com/linux/$(. /etc/os-release; echo "$ID")/gpg | apt-key add -

# These are dependencies to allow add-apt-repository.
RUN apt-get update && apt-get install -y software-properties-common python-software-properties
RUN add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/$(. /etc/os-release; echo "$ID") $(lsb_release -cs) stable"

# Install docker.
RUN apt-get update && apt-get install -y docker-ce

# Install node, npm, and elm.
RUN apt-get install -y nodejs
RUN npm install -g elm
RUN npm install -g elm-format
RUN npm install -g elm-test

# The purpose of this libsysconfcpus is supposedly to make elm compile faster.
RUN git clone https://github.com/obmarg/libsysconfcpus.git;
RUN cd libsysconfcpus && ./configure && make && make install && cd ../

# Install the `gloud` utility.
RUN curl -sSL https://sdk.cloud.google.com | bash
