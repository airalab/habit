## Telegram bot with Haskell

[![Build Status](https://travis-ci.org/akru/telegram-bot.svg?branch=master)](https://travis-ci.org/akru/telegram-bot)

### Install

    $ git clone https://github.com/akru/telegram-bot && cd telegram-bot
    $ stack setup
    $ stack ghci

### Run your story

The `Story` is an abstraction about sparsed data getted from user
though dialogue.

```haskell
helloStory :: Story
helloStory _ = hello <$> question "How your name?"
                     <*> question "How your surname?"
                     <*> question "How old are you?"
```

As you see the story handler `hello` is apply though the questions
to user responses.

```haskell
type Name    = Text
type Surname = Text
type Age     = Int

hello :: Monad m => Name -> Surname -> Age -> m BotMessage
hello name surname age = do
    return . toMessage $ "Hello, " <> name <> " " <> surname <> "!\n"
                      <> "You lost " <> (pack $ show age) <> " years =)"
```

To run the `Story` simple pass it to `storyBot` as value of mapping between
command an story.

```haskell
main :: IO ()
main = runBot config $ do
            storyBot helpMessage [("/hello", helloStory)]
  where config = defaultConfig { token = Token "bot..." }
```

Full example [text](examples/Hello.hs).
