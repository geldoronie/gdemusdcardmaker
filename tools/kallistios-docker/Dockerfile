FROM debian:10
ENV TZ=
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt update -qq
RUN apt install sudo -y
RUN useradd dreamcast
RUN adduser dreamcast sudo
RUN mkdir -p /home/dreamcast/game
ADD ./kos_setup_script.sh /home/dreamcast/
RUN chmod +x /home/dreamcast/kos_setup_script.sh
ADD ./docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh
RUN echo "dreamcast ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers
RUN chown -R dreamcast:dreamcast /home/dreamcast/
USER dreamcast
WORKDIR /home/dreamcast/
RUN bash -c "/home/dreamcast/kos_setup_script.sh"
RUN bash -c "source /opt/toolchains/dc/kos/environ.sh"
ENTRYPOINT [ "/docker-entrypoint.sh" ]