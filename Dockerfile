FROM haskell:8

ADD . /opt/irc_bot
WORKDIR /opt/irc_bot

RUN cabal update && \
    cabal install network time split

CMD ["runhaskell", "irc-bot.hs"]
