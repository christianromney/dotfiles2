<h1 align="center">
  <img src="https://i.imgur.com/0LElCjU.png" alt="done" width="300"></a>
  <br>
</h1>

<h4 align="center">A <a href="https://fishshell.com/">fish shell</a> package to automatically receive notifications when long processes finish.</h4>

<p align="center">
  <img src="https://img.shields.io/badge/stability-stable-green.svg" alt="Stability: Stable">
  <img src="https://img.shields.io/github/release/franciscolourenco/done.svg" alt="Release version">
  <img src="https://img.shields.io/badge/fish-%3E=2.3.0-orange.svg" alt="fish >=2.3.0">
  <img src="https://img.shields.io/badge/license-MIT-lightgray.svg" alt="License: MIT">
</p>
<br>

Just go on with your normal life. You will get a notification when a process takes more than 5 seconds finish, and the terminal window not in the foreground.

After installing you could type, for instance `sleep 6`, and start using other app. After 6 seconds you should get a notification.

## Installation

#### Using [Fisher](https://github.com/jorgebucaran/fisher)

```fish
fisher add franciscolourenco/done
```

#### Manually

```fish
curl -Lo ~/.config/fish/functions/humanize_duration.fish --create-dirs https://raw.githubusercontent.com/fishpkg/fish-humanize-duration/master/humanize_duration.fish
curl -Lo ~/.config/fish/conf.d/done.fish --create-dirs https://raw.githubusercontent.com/franciscolourenco/done/master/conf.d/done.fish
```

## Dependencies

If you want notifications with icons on macOS, please install `terminal-notifier`

```fish
brew install terminal-notifier
```

## Updating

```fish
fisher
```

[Subscribe](http://eepurl.com/cAcU3P) to the newsletter to be notified of new versions.

## Settings

#### Only display notifications if a command takes more than a certain amount of time

```fish
`set -U __done_min_cmd_duration 5000  # default: 5000 ms`
```

#### Prevent specific commands from triggering notifications. Accepts a regex.

This is useful to exclude commands like `git commit` for instance, since it could trigger unwanted notifications if it is configured to use an external editor.

```fish

set -U __done_exclude 'git (?!push|pull)'  # default: all git commands, except push and pull. accepts a regex.
```

#### Execute a custom command instead of showing the default notifications

```fish
set -U __done_notification_command 'some custom command'
```

## Support

- [fish](https://fishshell.com) 2.3.0+
- macOS 10.8+ via Notification Center.
- Linux via `notify-send`. Otherwise bell sound is played.
- Windows: Upvote https://github.com/franciscolourenco/done/issues/5 if interested.

## Credits

- [Francisco Lourenço](https://github.com/aristidesfl/) - Maintainer
- [Daniel Wehner](https://dawehner.github.io/) - Proof of Concept
- [Thanh Duc Nguyen](http://iamthanh.com/) - Logo

## License

Done is MIT licensed. See [LICENSE](LICENSE) for details.
