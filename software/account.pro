/**
Financial accounting.
*/

:- multifile prefix_description/2.

/**
account_unit_description(Name, Unit, Description)

Name is a number from your chart of accounts.

Unit is how much one unit of value in the account is worth.
Example: A Unit of usd/100 means that every unit in the account is worth 1 US cent (1/100 USD).

Example:

account_unit_description(1000, usd/100, 'Cash USD at Unknown Bank account 123456').
*/
:- multifile account_unit_description/3.

/**
transaction(Time, Description, Parts).

Time is second since Unix epoch?

Description is for human consumption.

Parts is a list.
Each element has the shape AccountName : Change.
Change must be an integer.
The changes should sum to zero.
*/
:- multifile transaction/3.
