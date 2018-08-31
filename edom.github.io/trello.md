---
title: Using Trello
permalink: /trello.html
date: 2018-05-15 23:54:00 +0700
---

- How should we best use Trello?
    - A card gathers people to help each other accomplish a common goal.
    - A card disseminates information from the people who have it to the people who need it.
- Traps
    - Because Trello cannot collaborative edit, we must not write too long before we save. Two people must not edit the card at the same time.
- What do we use Trello for?
    - Prioritize what to do, starting with the highest velocity (value per effort)
    - Give enough information for others to help us
- What Trello is not
    - A Trello column is not a mere to-do list.
    - Milestone lists are better than to-do lists.
    Be declarative, not imperative. Describe what you want, not how you want to do it.
    - If you can do it in a few hours, you don't need to make a Trello card for it.
    - If you don't need help with something, don't write a Trello card for it.
- Our attitude towards Trello
    - We should consider the time spent on Trello as overhead.
    We should minimize the time we are editing, moving, arranging Trello cards.
        - Human nature trap: It's fun to look busy (over-organizing).
- When not Slack?
    - If you need to remember it later, use Trello or Confluence, not Slack.
    - If a problem will take some time, don't waste your time typing on Slack.
    Put it in Trello, and assign it to the person who can fix it.
- What is in a card?
    - Write a card with the intention of helping everyone else help you.
    The card must answer "What can I help? How can I help?".
    - A card contains information or will contain information.
    - A person should be in the card if:
        - he person needs that information, or
        - he can contribute that information (he may also contribute in a comment).
    - No other person should care about that card.
- What is Trello?
    - To answer this, we answer these questions:
        - What does Trello make easy?
        - What does Trello make hard?
        - We seek an operational definition for Trello. We define something by what it do.
- What should we group together? Why?
    - A board is a list of a column.
    - A column is a list of cards. Trello uses the term "list", but we use "column" to avoid confusion.
    - A card has zero or more members.
    - A card has zero or more labels.
    - A label is like an atom in propositional logic.
    - The filtering system is a limited form of propositional logic.
    - A card is a way for several people to share information.
    - If two people need the same information, they should subscribe to the same card.
    - If we need two people to finish (archive) a card, then both of those people should be members of that card.
    - What is a label?
    You give a label L to a card if and only if you often need to filter (select) all cards labeled L.
    - Why are two cards in the same list?
    Because they have something in common. They share something. An aspect of them is equal.
    - We group something in a list to minimize moving cards.
    - What is that aspect? What irreplaceable advantage does it give us?
    - Because they have the same members? (We can use filter for this.)
    Because they belong to the same team?
    Because they have the same due date? (We can use filter for this.)
    Because they are a part of the same user story?
    Navigating multiple boards is hard (big cognitive burden).
    - Label is for filtering.
- What is easy to do in Trello?
    - Toggle any of the first 10 labels.
    - Show cards assigned to me (the user who is logged in).
    - Show cards by a conjunction or a disjunction of a Condition.
    - Condition is an element of Conditions.
    - Conditions is the union of all labels in a board and members of a board.
    - Click on the due date in the card description to mark it complete.
    - Read card comments in reverse chronological order (newer comments first).
    - Move a card to the bottom of an adjacent list.
    - See the number of checked items in a checklist in a card.
    - In the comment of a card, add another card.
- Trello assumes that every checklist in the item has the same effort.
Therefore we must make sure that every item in a checklist in the item has the same effort.
- What is a bit hard to do (because it cannot be done by keyboard alone):
    - Adding a check list in a card
    - A card can have many checklists
    - Marking a check list in a card
    - Unmarking a check list in a card
    - Filtering makes navigating a large board possible.
- What is hard to do in Trello:
    - Move from a board to another boards. (High cognitive load due to context switching.)
    - One person should not be in more than one board.
    - Move a card from a list to another list. It is easier to archive the list.
    - Unarchive a card
- What is impossible (assuming no plugins):
    - See due dates in non-US format
    - Use keyboard to move card up or down in a list
    - Gantt chart
    - Calendars
    - Progress report
    - Collaboratively edit a card description.
    Trello will only show the last saved description.
    The previous descriptions are not lost, but hardly accessible.
    The old description can be accessed by Exporting a card as JSON.
    - In a card description, we can link to a board or a card, but not link to a list.
- Member vs subscribe:
    - If and only if you are a member of a card, it will show in 'my card' filter (Press Q)
    The members of a card are the people assigned to a card
    We must tailor our workflow so that we use only the easy things.
- Please read and memorize the Trello shortcut keys.
    - Also read [How to use Trello like a pro](https://help.trello.com/article/734-how-to-use-trello-like-a-pro).
    - Those documents reflect what the Trello designer thinks Trello is best used for.
- In search of a Trello architecture
    - Fixed Pipeline / Mini waterfalls
        - Column = team, Card = product
        - One column is assembly line.
        - Cards move from left to right.
        - Progress is sequential. Right column cannot start before left column finishes what it has to do with the card.
        - Ideal for the same process that is repeated very many times.
        - For example, in our case, the columns would be Requirement, UI, Database, Backend, Frontend, Done.
    - Column = a rather big to-do, card = breakdown of the column
        - For example, in our case, each column would be "As a (who), I (do what)".
        - A column is a work item and each card in the column is a breakdown of that item.
    - Column = epic / bigger user story, card = smaller user story
    - Column = milestone, card = breakdown of milestone
- Trello list hierarchy (a Trello account is a list of lists of ...)
    - A Trello account is a list of boards.
    - A board is a list of columns.
    - A column is a list of cards.
    - A card is a list of checklists.
    - A checklist is a list of checklist items.
    - Therefore we can use Trello for work breakdown structure of perhaps at most 4 levels deep.
