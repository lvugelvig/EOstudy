# Before doing anything

Make sure that your directory is up-to-date, in particular, that you have downloaded the `_summary.md` files edited by coworkers. Type
```{sh}
git pull origin master
```
to download the potential changes.


# Get data from the evoldir website (Flo)

## New version (Apr 2017 onwards)
type
```
./getlistYM Year Month
```
where `Year` is the year (e.g., 2017) and `Month` is the three letter version of the month (Jan, Fev, ...).

This (replace `Year` by the corresponding year and `Month` by the 3-letter month)
- creates a `.csv` file `ads_Year-Month.csv` listing all the ads, their date and type (Conferences/WorkshopsCourses).
- downloads all the texts of the ads, stored in a folder `ads/Year/Month/`.
- copies the list of ads in the working directory, to later include it in the file with all data.
- creates `_summary.md` file for each ad, that will be filled in later.

Then synchronize with Github.


## Old version (Apr 2016 - Mar 2017)
type
```
./getlist Month
```
where `Month` is the three letter version of the month (Jan, Fev, ...).

This (replace `Month` by the 3-letter month)
- creates a `.csv` file `ads_Month.csv` listing all the ads, their date and type (Conferences/WorkshopsCourses).
- downloads all the texts of the ads, stored in a folder `ads/Month/`.
- copies the list of ads in the working directory, to later include it in the file with all data.
- creates `_summary.md` file for each ad, that will be filled in later.

Then synchronize with Github. May need to first
```{sh}
git pull origin master
```
to download the changes in `_summary.md` files done by coworkers.

# Compile evoldir data

## Tasks

 1) Add the ads in the AllData file on Google Drive (Flo)
   Copy-paste the list generated with the above-mentionned script ad the end of the table.

 2) Fill in the summary file

  In the `_summary.md` file for each ad (in `Evoldir/ads/MON`, where MON is 3-letter month), compile all the names of speakers and organisers that you can find in the evoldir ad and on the ad website (remove *, - and superfluous things to end up with clean lists with one name per line)

 3) Fill in the Data set

   For each ad, fill in the AllData table in Google drive (see below)

## Hesitations

** If you have any hesitation about an entry, please flag it with a bright background color** (e.g., yellow, red, but do not use gray which is reserved for non-included ads).

## Inclusion rule

Set include = FALSE if

 - event is a duplicate (has already been announced and is hence already listed in the AllData table);
 - no information on invited speakers/info at all (neither in ad nor website; happens for instance for "save the date" type of ads);
 - if Course: if run by private company like prstatistics (we want to focus on "academic" events).

**AND** do put a comment in the Comments column (last one) to justify the decision.

Note: Do include Courses even if there are only instructors listed. The `Type` column is here to help distinguish between Courses, Workshops and Conferences, and hence the different types of "invited speakers".

## Organizers

If no single piece of information is found about the organizers (neither SciComm nor Organizers), enter zeros ("0") in the `Org_nb` and `Org_W` columns. We need to always fill in entries for the organizers, otherwise the columns won't appear in the questionnaire and the contact person will not have a chance to enter the information.

## Contents of AllData

Column | Description
-------|-------------
Ad_ID | ID in the database; evoldir email subject (entered automatically)
Ad_date | Date evoldir email was sent (entered automatically)
Ad_Type | evoldir category (entered automatically)
Date_collection | date at which the rest of the data for this line was entered
Name_collector (F/N/S/L) | Initial of the person who entered these data
Short_name | Short name for the event, **will be email subject**, so be concise and precise
Type | Type of event (Symposium, Conference, Workshop, Course, Summer School), **will be in email**
Include | whether to include in the study/send email. If FALSE, detail in Comments
Location | Country where event is taking place
Website | website address of the event **will be in the questionnaire**
Org_country | Country of the main organizer
Invited_nb | Total number of featured speakers (keynote+invited)
Invited_W	| Number of female featured speakers (keynote+invited)
Instructor_nb | Total number of instructors (in case of course)
Instructor_W | Number of female instructors (in case of course)
Org_nb | Total number of organizers (either local org. committee, or unspecified). *Enter 0 if no information at all about any kind of organizers (not NA)*.
Org_W	| Number of female organizers *Enter 0 if no info (see line above)*
SciCom_nb | Total number of people in Scientific Committee (in case it is specified)
SciCom_W | Number of women in scientific committee (in case it is specified)
Org_name1 | Last name of the main organizer
Org_email1	| email address of the  main organizer
Org_name2 | Last name of another organizer
Org_email2 | email of this other organizer
Month	| 3-letter month (extracted from Ad_date), for sorting
Comments | In particular decision not to include
Date_emailsent | date at which emails were sent
Org_reply1 | Whether organizer replied	after the first email
Date_relance | Date at which 2nd email was sent
Org_reply2 | Whether organizer replied after second email
Timestamp	| *data from answers file:* date of reply
Event name | *data from answers file:* code of event (quality check, has to be the same as Ad_ID)
Question 0 | *data from answers file:*
New_Invited_nb	| *data from answers file*
New_Invited_W | *data from answers file*
New_Instructor_nb | *data from answers file*
New_Instructor_W | *data from answers file*
New_Org_nb | *data from answers file*
New_Org_W | *data from answers file*
New_SciCom_nb | *data from answers file*
New_SciCom_W | *data from answers file*
Question 1 | *data from answers file*
Question 2 | *data from answers file*
Question 3 | *data from answers file*
Question 4 | *data from answers file*
Question 5 | *data from answers file*
KeepInformed | *data from answers file*
