#Rules 


The following attemps to document the rules that apply to the various parts of a Tweet. It is based purely on observation; as of now
I haven't found any official documentation that lays down the "official" rules. 

### Hashtags
---

A hashtag is a string of characters beginning with a '#' character followed by one or more alphanumeric characters, terminated by either a space or end of string. It must contain at least one letter. A hashtag may not contain another hash character aside from the initial character. 

The following table summarizes the rules:

| No  | Yes |
|-----|-----|
| #1  | #a  |
| #1# | #1a |
| #   |     |
| a   |     |
| #a@ |     |
