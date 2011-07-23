=========================
What is cl-odesk?
=========================
This is cl-odesk, an implementation of the oDesk API in Common Lisp.

Spec URI: http://developers.odesk.com

=========================
Example Usage
=========================

If you already installed cl-odesk you can load it by::

 (require :odesk)

Before doing some API calls first you need to define connection. You will need PUBLIC_KEY, PRIVATE_KEY and TOKEN. The last one you can get only by authenticating yourself in your favourite web browser. Please read API Documentation at http://developers.odesk.com

You can create connection by instantiating API class::

 (defparameter *test-connection*
   (make-instance 'odesk:api-json
                  :public-key "PUBLIC"
                  :secret-key "SECRET"
                  :api-token "TOKEN"))

Here is macro that can ease your life a lot::

 (with-odesk (:connection con
              :public-key "PUBLIC"
              :secret-key "SECRET"
              :api-token "TOKEN")
   (print con))

And another macro that can help too::

 (connect-odesk (:public-key "PUBLIC"
                 :secret-key "SECRET"
                 :api-token "TOKEN"))

Now let's do request to get info about user.
If you went first way and created variable *test-connection*::

 (odesk:hr/get-user :connection *test-connection*)

If you go the second way and will use with-odesk macro. Here you have two options. The first one::

 (odesk:with-odesk
     (:connection my-con
      :public-key "PUBLIC"
      :secret-key "SECRET"
      :api-token "TOKEN")
   (odesk:hr/get-user :connection my-con))

The second::

 (odesk:with-odesk
     (:public-key "PUBLIC"
      :secret-key "SECRET"
      :api-token "TOKEN")
   (odesk:hr/get-user))

Third way is for those who want to create connection globally and then use it in the code with-out the need to use with-odesk macro::

 (odesk:connect-odesk (:public-key "PUBLIC"
                       :secret-key "SECRET"
                       :api-token "TOKEN"))
 (odesk:hr/get-user)

=========================
Supported API Calls
=========================

Authentication via Token
-----------------
* Get Token::
  auth/get-token (tested)
* Get frob::
  auth/get-frob (not tested)
* Check token::
  auth/check-token (tested)
* Revoke Session::
  auth/revoke-token (tested)

Authentication via OAuth
-----------------
no support at all.

Financial Reporting
-----------------
* Generate Billing Reports for a Specific Provider::
  no support
* Generate Billing Reports for a Specific Provider's Team::
  no support
* Generate Billing Reports for a Specific Provider's Company::
  no support
* Generate Earning Reports for a Specific Provider::
  no support
* Generate Earning Reports for a Specific Provider's Team::
  no support
* Generate Earning Reports for a Specific Provider's Company::
  no support
* Generate Billing Reports for a Specific Buyer's Team::
  no support
* Generate Billing Reports for a Specific Buyer's Company::
  no support
* Generate Earning Reports for a Specific Buyer's Team::
  no support
* Generate Earning Reports for a Specific Buyer's Company::
  no support
* Generate Financial Reports for a Specific Account::
  no support
* Generate Financial Reports for an owned Account::
  no support

Custom Payments
-----------------

* Custom Payment::
  no support

Hiring. User Roles
-----------------

* User Roles::
  no support
* Referenced user::
  hr/get-user (tested)

Hiring. Jobs HR API
-----------------

* Returns all jobs that a user has manage_recruiting accesss to::
  no support
* Get Job by reference::
  no support
* Post a Job::
  no support
* Update Job::
  no support
* Cancel the Job::
  no support

Hirigin. Offers API
-----------------

* Lists all the offers on a specific job or within a specific team::
  no support
* Get Offer::
  no support

Hiring. Engagements API
-----------------

* Return engagement(s) based on the parameters supplied in the API call::
  no support
* Get Engagement::
  no support

Message Center
-----------------

* Retrieve a list of all active trays and a message count for each::
  mc/get-trays (tested)
* Retrieve tray contents::
  mc/list-tray (tested)
* List thread details based on thread id::
  no support
* Update threads based on user actions::
  no support
* Send a message to a user::
  no support

oConomy Reports API
-----------------

* Generate Monthly oDesk job market report::
  no support
* Generate Hours worked by location report::
  no support
* Generate oConomy weekly growth report::
  no support
* Generate Top countries by hours worked for last 30 days report::
  no support
* Generate Earnings by category report::
  no support
* Generate Monthly most requested skills report::
  no support

Organization
-----------------

* User Information::
  hr/get-myself (tested)
* Company Information::
  hr/get-companies (tested)
  hr/get-company (not tested)
  hr/get-company-teams (tested)
  hr/get-company-users (not tested)
* Team Information::
  hr/get-teams (tested)
  hr/get-team-users (not tested)

Provider Profile
-----------------

* Provider Information::
  profiles/get-provider (tested)
  profiles/get-provider-brief (tested)

Search Jobs
-----------------

* Searching Jobs::
  profiles/get-jobs (tested)

Search Providers
-----------------

* Searching Providers::
  profiles/get-providers (tested)

Snapshot
-----------------

* Get Snapshot::
  no support
* Update Snapshot Memo::
  no support
* Delete Snapshot::
  no support

oTasks
-----------------

* List all Tasks within a Company, Team or User::
  no support
* List all oTask records within a Company, Team or User::
  no support
* Return a specific task record within a company, team or user::
  no support
* Create an oTask record within a company, team or user::
  no support
* Update an oTask record within a company, team or user::
  no support
* Delete an oTask record within a company, team or user::
  no support
* Delete All oTask records within a company, team or user::
  no support
* Update a group of oTask records within a company, team or user::
  no support

Team
-----------------
* Get Team Rooms::
  team/get-teamrooms (tested)
* Get Team Room::
  team/get-teamroom (tested)

Work Diary
-----------------
* Get Work Diary::
  team/get-workdiary (tested)

Time Reports
-----------------
* Generate Time Reports for a Specific Team::
  timereports/get-team (tested)
* Generating Company Wide Reports::
  timereports/get-company (tested)
* Generating Agency Specific Reports::
  timereports/get-agency (tested)
* Generating Provider Specific Reports::
  timereports/get-provider (tested)



