FROM haskell:8

ADD . /opt/irc_bot
WORKDIR /opt/irc_bot

RUN cabal update && \
    cabal install --lib network time split mtl

CMD ["runhaskell", "irc-bot.hs"]
