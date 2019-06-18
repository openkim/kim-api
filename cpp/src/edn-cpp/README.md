edn-cpp
=======

edn lib for c++. 

##intro
Useful for parsing [edn](http://github.com/edn-format/edn). read returns an `edn::EdnNode`. There are three cases to consider: 

* If it is an atom (nil, symbol, keyword, bool, int, float, string, char) it will have a `value` property which is a string you can then cast/coerce how you please. 

* If it is a collection (list, map, vector, set) it will have a `values` property which is itself a list of `edn::EdnNode` items which you can then walk via what ever mechanism and do with them what you will. 

* If it is a tagged item you will have the raw tagname in the `value` property and then the `values` property will be a list containing the `edn::EdnNode` representing the tagname and another `edn::EdnNode` containing the value for the tag. 

In general `edn-cpp` makes no assumptions about how you want to reify the data - it just guarantees you have well formed edn node(s) which you can handle how ever you like. 


##using

	#//define DEBUG
	#include "edn.hpp"
	#include <iostream>
	
	using edn::EdnNode;
	using edn::read;
	using edn::pprint;
	
	int main() {
		try {
			EdnNode someMap = read("{:some :map :with [a vector of symbols]}");
			std::cout << pprint(someMap) << std::endl;
		} catch (const char* e) {
			std::cout << "Error parsing: " << e << std::endl;
		}
	}

will output: 

	{:some :map :with [a vector of symbols]}

And with debug uncommented: 

	<EdnMap {<EdnKeyword :some> <EdnKeyword :map> 
	         <EdnKeyword :with> <EdnVector [<EdnSymbol a> 
	                                        <EdnSymbol vector> 
	                                        <EdnSymbol of> 
	                                        <EdnSymbol symbols>]>}>

##debug
If you define DEBUG you will see debug output when using pprint. This looks like:

	<EdnMap {
	  <EdnKeyword :x> <EdnKeyword :y> 
	  <EdnKeyword :z> <EdnVector [<EdnInt 1> 
	                              <EdnInt 2> 
	                              <EdnInt 3> 
	                              <EdnTagged #<EdnSymbol tagged/item> 
	                                         <EdnString "cats-rabbits-bears">>]>}>

Which is nice when proving things are parsing as you expect. 

##api
	list<EdnToken> lex(string ednString)
	
	EdnNode read(string ednString)

	string pprint(EdnNode node)
	
	bool validSymbol(string value)
	
	bool validKeyword(string value)

	bool validNil(string value)
		
	bool validBool(string value)
	
	bool validInt(string value)
	
	bool validFloat(string value)
	
	bool validChar(string value)
	
	EdnNode handleAtom(EdnToken token)
	
	EdnNode handleCollection(EdnToken token, list<EdnNode> values)
	
	EdnNode handleTagged(EdnToken token, EdnNode value)
	
##structs
	EdnToken
		TokenType type
		int line
		string value
	
	EdnNode
		EdnToken type
		int line
		string value         #used for atoms
		list<EdnNode> values #used for collections
		
##enums
	TokenType
		TokenString
		TokenAtom
		TokenParen
		
	NodeType
		EdnNil
		EdnSymbol
		EdnKeyword
		EdnBool
		EdnInt
		EdnFloat
		EdnString
		EdnChar
		EdnList
		EdnVector
		EdnMap
		EdnSet
		EdnDiscard
		EdnTagged
		
		
##todo
make pprint actually format as demonstrated above. Right now it is just a single string with out the indentation. 