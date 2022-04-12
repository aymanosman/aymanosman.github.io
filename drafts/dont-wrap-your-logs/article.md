Elixir comes with a powerful and convenient logging facility exposed by the `Logger` module. It is used universally in Elixir projects, which makes the subject of logging far more approachable in Elixir than in most other languages, where the logging story is far less straightforward.

Basic usage of `Logger` may be enough for you, most of the time, but, sometimes, the need to customise some aspects of it will arise.

One way of achieving this is to wrap the lower level logging module. This is a completely natural thing to do. As a project grows, we will often wrap lower level functionality in order to enforce conventions and abstract repetitive code.

So you would think that it would be completely reasonable to wrap Elixir's `Logger` module, if the need to customise logging were to arise.

Here is the problem: you start to implement this solution by creating the following wrapper module:

```elixir
defmodule MyLogger do
  require Logger
  def info(message, metadata \\ []) do
    Logger.info("[myprefix] " <> message, metadata)
  end
end
```

At first, everything seems to be working fine. You call this new wrapper function from another module and confirm that it is working as expected

```elixir
defmodule Example do
  def work() do
    MyLogger.info("working...")
  end
end
```

```
iex(2)> Example.work

11:38:16.428 [info]  [myprefix] working ...
```

The problem is discovered when you decide to include source location information alongside the ordinary log message.

```elixir
import Config

config :logger, :console,
  metadata: [:file, :function, :module]
```

```
iex(1)> Example.work

14:00:03.912 file=lib/my_logger.ex function=info/2 module=MyLogger [info]  [myprefix] working ...
```

As you can see, every log line reports the source of the log event as our new wrapping code. This is not what you wanted.

## What is the problem?

The problem is the Logging API functions, such as `Logger.info` and `Logger.debug`, are not functions but macros, and, in this context, it matters where you call them.

Macros know in what files they are called, they have access to that information through the `__CALLER__` special form. In fact, that is exactly how `Logger` captures this information.

By placing the `Logger.info` call in one place (and calling it in a function), you have inadvertently signalled to the logging system that all log events originate from that one location.

## What is the solution?

Don't wrap the logging macros at all. You can probably achieve what you want by some other means. Look at the documentation for [`Custom Formatting`](https://hexdocs.pm/logger/1.13/Logger.Backends.Console.html#module-custom-formatting) to customise how a log event is serialised into a log line. If the Elixir logger is not flexible enough for you, consider looking at the lower level [Erlang Logger](https://www.erlang.org/doc/apps/kernel/logger_chapter.html) for options.

## I really want to wrap logging calls

If you insist on wrapping calls to the Elixir logger, then wrap it in a macro.

```elixir
defmodule MyLogger do
  # I don't recommend doing this
  defmacro info(message, metadata \\ []) do
    quote do
      require Logger
      Logger.info("[myprefix] " <> unquote(message), unquote(metadata))
    end
  end
end

```

```
iex(1)> Example.work

13:09:48.324 file=lib/example.ex function=work/0 module=Example [info]  [myprefix] working ...
```

The reason this works is that the call to the macro `Logger.info/2` now expands in the context of the call to `MyLogger.info/2`, and so, source location information is preserved.

As a demonstration of the kind of information that is available to a macro as it expands, here is an alternative macro

```elixir
defmodule MyLogger do
  defmacro info(message, metadata \\ []) do
    env = __CALLER__

    quote bind_quoted: [
            message: message,
            metadata: metadata,
            file: env.file,
            line: env.line
          ] do
      require Logger

      Logger.info("source=#{Path.basename(file)}:#{line} #{message}", metadata)
    end
  end
end

```

```
iex(3)> Example.work

13:20:14.575 [info]  source=example.ex:5 working ...
```
