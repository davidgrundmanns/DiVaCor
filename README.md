# DiVaCor

### Links

#### On YouTube:
[![Video Introduction to the DiVaCor Dashboard on YouTube](https://i3.ytimg.com/vi/ouZwqLNO--s/maxresdefault.jpg)](https://www.youtube.com/watch?v=ouZwqLNO--s)

#### Further links:
- [on devpost.com](https://devpost.com/software/divacor?_gl=1*185l777*_ga*MTg0NjczMDQ5OC4xNzYxNzUyNjcx*_ga_0YHJK3Y10M*czE3NjE3NTI2NzEkbzEkZzAkdDE3NjE3NTI2NzEkajYwJGwwJGgw)

Vorstellung der Platform:
- [Open-Source-Plattform unterstützt Exit aus dem Corona-Lockdown, 12.04.2020 von André Walter on industry-of-things.de](https://www.industry-of-things.de/open-source-plattform-unterstuetzt-exit-aus-dem-corona-lockdown-a-836f1bd74c3e0a903e378547e80978cc/)

---

### Info ([from devpost.com by Lars Geißler](https://devpost.com/software/divacor?_gl=1*185l777*_ga*MTg0NjczMDQ5OC4xNzYxNzUyNjcx*_ga_0YHJK3Y10M*czE3NjE3NTI2NzEkbzEkZzAkdDE3NjE3NTI2NzEkajYwJGwwJGgw))

#### Inspiration

Our team is coming from the German Hackathon #WirvsVirus and got inspired by our passion for data, digitization and social commitment. At the moment, all measures that serve to contain the spread of the virus are generalized and independent from local conditions such as population composition, urbanization level, local economy, medical infrastructure, sources of infection and R0 on district-level. This results in unnecessary burden and unpredictable economic behaviour caused by disproportional measures.
What it does

The current situation increases the pressure for political and economic decision makers. They are intended to prevent a medical catastrophe without conjuring up an economic one, the consequences of which are also life-threatening - and this with unclear data regarding the level of danger and mortality of COVID-19. Decison makers should be enabled to define and manage measures on smallest district level, either centralized via the German government but also through the administrative bodies itself. In addition, the impact of measures on the curve of infections and economic development is just not clear. Our project DiVaCor supports a clear answer to these questions.

- Attention: Sample data is the most precise for Germany! -

#### How I built it

Teamwork and long hours of work

#### Challenges I ran into

Access to relevant data sources Finding skilled team members Marketing and PR Access to decision makers

#### Accomplishments that I'm proud of

We created the opportunity to even display the R0 on district level. As per our knowledge, this is unique! Check it out on our platform: https://divacor.org/

#### What I learned

The more you are specialized, the harder it is to find an expert and mentor. We entered the Hackathon with the expectation to get into contact to political decision makers or people with access to the target group and unfortunately, this did not happen.

#### What's next for DiVaCor

A service was set up which allows local media a free-of-charge access to statistics on the curve of infections of their relevant district compared to the state and/or federal state. In addition, to give the solution attention, contact to political decision makers to further spread the word and reach target groups is what we are aiming for.

#### Built With

directus, java11, r

---

### Proposal (German)

#### Abstract
Der COVID-19 Erreger breitet sich weltweit exponentiell aus. Regierungen reagieren mit vielseitigen Maßnahmen auf die Ausbreitung. Das Robert-Koch-Institut strebt die Steuerung der Ausbreitung an, um die „Überlastung der Versorgungssysteme [zu vermeiden]“ ([Source](https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Ergaenzung_Pandemieplan_Covid.pdf;jsessionid=58D3EF8450D64033FE20692EE755E745.internet061?__blob=publicationFile)). Um zu verstehen, wie die Ausbreitung von COVID-19 unter maßvoller Einschränkung der Freiheiten der Bevölkerung gelingen kann, bedarf es inferenzstatistischer Analysen. Diese Analysen ermöglichen informative Vorhersagen über die Einflüsse von medizinischer Infrastruktur, Bevölkerungsstruktur, gesellschaftlichen und politischen Maßnahmen auf die Eindämmung von COVID-19. Mit den Ergebnissen können politische Entscheidungsträger gezielter reagieren und die Öffentlichkeit besser informiert werden.  

#### Intro
Voraussetzung für solche Vorhersagen sind zwei Aspekte: 1. Gesicherte und berechtigte Datenzugänge zu qualitativ hochwendigen Daten; 2. Verknüpfungen der großen Datenmengen. 

Abhängige Variablen. Derzeit arbeiten wir auf Basis der Informationen des RKI, der Johns Hopkins University und des European Centers for Disease Control. Wir können folglich nur die durch diese Institutionen offiziell gemeldeten Fallzahlen verwenden. Ziel ist es diese Daten zu verwenden um zum einen vorherzusagen wie sich die Inzidenzrate entwickelt hätte, wären keine Maßnahmen ergriffen worden (Kontrollgruppe); zum anderen wollen wir ermitteln wie sich die Inzidenzrate tatsächlich entwickelt hat (Treatmentgruppe). Die Datenqualität der durch das RKI veröffentlichen Meldedaten könnte dabei besser ausfallen. Die Tagesschau und das ZDF heute berichtete zuletzt z.B. wiederholt die Fallzahlen der Johns Hopkins University und nicht die Fallzahlen des RKI. Es wäre insbesondere hilfreich das Meldesystem zu beschleunigen und sicherzustellen, dass es nicht zu Meldeausfällen kommt ([siehe auch spiegel.de](https://www.spiegel.de/wissenschaft/coronavirus-wie-belastbar-sind-die-rki-daten-a-13bd06d7-22a1-4b3d-af23-ff43e5e8abd6)).  Darüber hinaus wäre es hilfreich mehr Individualdaten zu generieren, die über bloße Fallzahlmeldung hinaus gehen. Daten zum Verlauf der Krankheit, typischen oder atypischen Symptome, Heilung und Kontaktpersonen würden eine kontrolliertere und genauere Betrachtung ermöglichen.      

Unabhängige Variablen. Um den Effekt der politischen Interventionen zu verstehen greifen wir auf einen kleinen Datensatz, der die Interventionen dokumentiert zurück. An dieser Stelle währen weitere Daten hilfreich. Wir sind besonders interessiert an Daten zu politischen Maßnahmen: 
•	Wo sind diese aufgetreten? (welches Bundesland / welche Kommune) 
•	Was beinhaltet eine Maßnahme?
•	Wann wurde eine Maßnahme verkündet?
•	Wann tritt eine Maßnahme in kraft?
•	Wie lange gilt eine Maßnahme?
•	Bestenfalls: Bereitstellung von Daten auf Ebene der Landkreise / Kreisfreien Städte

Kontrollvariablen. Über die Interventionen hinaus gibt es bestimmte Faktoren von denen Ausgegangen wird, dass sie die Entwicklung der Inzidenzrat positiv oder negativ beeinflussen. Wir kontrollieren für diese Faktoren durch die Verknüpfung einer großen Menge an Daten; etwa durch Ergänzung der Bevölkerungsverteilung, Alter, Geschlecht, Krankenhäuser in einer Region, etc. Aktuell konzentrieren wir uns auf die Verknüpfung der RKI Daten mit Informationen auf der Ebene der NUTS3 Regionen in Deutschland, mit dem Ziel bei wachsender Inzidenz schnell Analysen auch für kleine Regionen zu entwickeln. In diesem Zusammenhang sammeln wir unter anderem: 
•	Ferienzeiten
•	Geschlecht 
•	Altersstruktur 
•	Bevölkerungszahlen und -dichte
•	Medizinische Infrastruktur: Anzahl von Krankenhausbetten per Capita
•	Medizinische Infrastruktur: Krankenhäusern, Kliniken, Hausärzten, Apotheken, Pflegeheime
•	Anzahl von Bahnhöfen und Reisezentren
•	Arbeitslosenrate
•	Schülerrate
•	Bestenfalls gebe es auch diese Daten auf Landkreis-Ebene / Kreisfreien Städte

Im Wesentlichen möchten wir so viele Daten wie möglich für die Ebene der NUTS3 Regionen gewinnen, die mit COVID-19 in Verbindung stehen könnten. Es gilt folglich permanent weitere Daten zu identifizieren. Von besonderen Interesse wären natürlich Daten wie: 
•	Vorerkrankung bei Menschen 
•	Aktuell gesunde Menschen 
•	Intergenerationale Haushalte (wo lebt alt und jung unter einem Dach)
•	Kinder- und Pflegebetreuung
•	Individualdaten von Personen [im Rahmen des Datenschutzes]

---

Originally available on divacor.org, this project is not live anymore.

