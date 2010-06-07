/***************************************************************************
                          documentstyle.h  -  description
                             -------------------
    begin                : Son Nov 10 2002
    copyright            : (C) 2002-2007 by Andre Simon
    email                : andre.simon1@gmx.de
 ***************************************************************************/


/*
This file is part of Highlight.

Highlight is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Highlight is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Highlight.  If not, see <http://www.gnu.org/licenses/>.
*/


#ifndef DOCUMENTSTYLE_H
#define DOCUMENTSTYLE_H

#include <string>
#include "configurationreader.h"
#include "elementstyle.h"
#include "stylecolour.h"

using namespace std;

namespace highlight
{

	/** maps keyword class names and the corresponding formatting information*/
	typedef map <string, ElementStyle> KeywordStyles;

	/** iterator for keyword styles*/
	typedef KeywordStyles::const_iterator KSIterator;

	/** \brief Contains information about document formatting properties.
	 * @author Andre Simon
	 */

	class DocumentStyle
	{
		private:
			ElementStyle comment, slcomment, str, dstr,
			escapeChar, number, directive, line, symbol;
			ElementStyle defaultElem;
			Colour  bgColour;
			Colour  markLineColour;

			string fontsize;
			bool fileFound;

			KeywordStyles keywordStyles;

		public:
			/** Constructor
			    \param styleDefinitionPath Style definition path */
			DocumentStyle ( const string & styleDefinitionPath );

			/** Constructor */
			DocumentStyle();
			~DocumentStyle();

			/** load style definition
			      \param styleDefinitionFile Style definition path
			      \return True if successfull */
			bool load ( const string & styleDefinitionFile );

			/** \return class names defined in the theme file */
			vector <string> getClassNames() const;

			/** \return keyword styles */
			KeywordStyles getKeywordStyles() const;

			/** \return Font size */
			string getFontSize() const;

			/** \return Background colour*/
			Colour getBgColour() const;

			/** \return Mark line colour*/
			Colour getMarkLineColour() const;

			/** \return Style of default (unrecognized) strings */
			ElementStyle getDefaultStyle() const;

			/** \return Comment style*/
			ElementStyle getCommentStyle() const;

			/** \return Single line comment style*/
			ElementStyle getSingleLineCommentStyle() const;

			/** \return String style*/
			ElementStyle getStringStyle() const;

			/** \return Directive line string style*/
			ElementStyle getDirectiveStringStyle() const;

			/** \return Escape character style*/
			ElementStyle getEscapeCharStyle() const;

			/** \return Number style*/
			ElementStyle getNumberStyle() const;

			/** \return Directive style*/
			ElementStyle getDirectiveStyle() const;

			/** \return Type style*/
			ElementStyle getTypeStyle() const;

			/** \return Line number style*/
			ElementStyle getLineStyle() const;

			/** \return Bracket style*/
			ElementStyle getSymbolStyle() const;

			/** \param className Name of keyword class (eg kwa, kwb, .., kwd)
			    \return keyword style of the given className
			*/
			ElementStyle getKeywordStyle ( const string &className ) ;

			/** \return True if language definition was found */
			bool found() const ;
	};

}

#endif
