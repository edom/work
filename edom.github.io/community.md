---
title: Building online communities
permalink: /community.html
date: 2018-07-29 17:08 +0700
---

- There is a problem in the Indonesian functional programmers community:
the community is fragmented because there are too many communication channels (messaging apps).
    - People choose whichever one easiest for them.
    - People use whichever one their friends use.
    - We can't force everyone to use one particular app.
    - Thus we have to bridge the apps.
- Is there a free online chat/messaging system that satisfies these?
    - Remembers message history.
    Can replay other people's messages sent while I'm offline.
    - Is convenient to use on desktop.
    - Is convenient to use on phone.
    - Doesn't require me to create an account, let alone give my phone number.
    - Somewhat secure.
- What are our choices?
    - IRC
    - Slack
        - Why not?
            - Slack yanked their XMPP/IRC support.
                - https://news.ycombinator.com/item?id=16539857
                    - - https://hn.svelte.technology/item/16539857
                - https://drewdevault.com/2015/11/01/Please-stop-using-slack.html
                    - https://news.ycombinator.com/item?id=10486541
                    - https://news.ycombinator.com/item?id=10486688
                    - https://news.ycombinator.com/item?id=10486541
                        - IRC isn't that good either.
                            - https://news.ycombinator.com/item?id=11013434
                                - Matrix?
                    - - https://medium.com/ignation/time-to-replace-slack-who-will-win-mattermost-or-riot-matrix-a090e9cdc219
                - https://it.slashdot.org/story/18/03/08/2049255/slack-is-shutting-down-its-irc-gateway
                    - http://shout-irc.com/
                - https://www.theregister.co.uk/2018/03/09/slack_cuts_ties_to_irc_and_xmpp/
                - https://www.reddit.com/r/linux/comments/8d3sg1/irc_gateway_for_slack/
                - https://www.reddit.com/r/Slack/comments/8hyky9/best_alternative_to_the_now_defunct_irc_gateway/
                - https://tedium.co/2017/10/17/irc-vs-slack-chat-history/
    - WhatsApp group
    - Facebook group
    - Google groups (mailing list) (Can we assume that everyone has a Google account?)
    - non-Google mailing list
    - Telegram
        - Can we use a virtual phone number?
            - https://www.reddit.com/r/Telegram/comments/3htffs/how_to_use_telegram_without_having_a_phone_number/
        - https://sameroom.io/blog/connect-slack-and-telegram/
            - https://sameroom.io/
                - Not free.
                - How trustworthy is this company?
                It will divulge everyone's email address.
        - https://github.com/FruitieX/teleirc
    - https://www.irccloud.com/
    - Usenet (news server)
    - Decentralized messaging?
    Distributed chat?
    Blockchain technology?
    Torrent-based messaging?
        - matrix.org?
        - https://github.com/orbitdb/orbit
    - Disqus?
- Which features do we want?
    - If we mention an offline user at Slack, it emails the user.
- Which one is the least hassle to user?
    - Slack has the best user experience.
    Slack makes IRC feels unacceptably shitty.
    IRC doesn't require you to give your email, but other people can know your IP address (and therefore geolocate it).
        - https://code.tutsplus.com/tutorials/irc-is-back-heres-your-starter-guide--net-31369
            - http://www.ircbeginner.com/ircinfo/irc-safety.html
- How is IRC undesirable?
    - Everyone might be able to see your IP address.
    This depends on how you connect though.
    Perhaps a gateway can hide your IP address, but it might also choose not to.
    - Because IRC doesn't require registration, it's easy to spam.
    - Message history isn't built-in.
- Why don't we just use email?
    - Email is reserved for low-volume important/serious traffic?
- Why don't we just use Disqus?
- IRC is great because it doesn't require you to divulge much private data before you can start using it.
    - But how do you use IRC conveniently from phone?
    - How do you replay messages posted while you're offline?
- There are two parties:
    - User
    - Channel operator
- For the user: using IRC.
    - Don't install anything on your computer/phone.
    Use an IRC web client such as [webchat.freenode.net](https://webchat.freenode.net/?channels=%23haskell-id).
    Pick any user name.
    - [How to IRC like it's 2017](https://anders.unix.se/2017/06/23/how-to-irc-like-its-2017/)
    - https://www.drupal.org/irc/usage
    - How do we tell botbot to replay past messages to me?
    We can't.
    We must use the web interface.
    - [botbot.me](https://botbot.me/)
        - How do I set up botbot to remember past messages (record channel history)?
- For the channel operator: setting up an IRC channel
    - How do I save message history?
    By using an IRC bouncer ([SO 21954022](https://stackoverflow.com/questions/21954022/internet-chat-service-like-irc-but-with-message-history)).
        - If the bouncer goes down or can't connect to the IRC server, every message posted while the bouncer is disconnected is lost forever from history.
            - No problem. Slack free-tier doesn't remember entire history either.
        - What are some free hosted IRC bouncers?
            - https://wiki.znc.in/Providers
            - others?
        - https://www.quora.com/How-do-I-search-for-previous-messages-on-an-IRC-channel
        - http://www.tomsguide.com/answers/id-2052675/irc-channel-history-connected.html
    - https://dague.net/2014/09/13/my-irc-proxy-setup/
- Hosting your community's static website
    - https://github.com/haskell-id/website/issues/5
    - Choices:
        - GitHub Pages
        - Firebase
