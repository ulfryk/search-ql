import { Either, Left, Right } from 'monet';
import { Failure } from 'parsimmon';

import { Expression } from './expressions';
import { ParseFailure } from './parse-failure';
import { ParserName, QueryParserFactory } from './parsers';

export const parseSearchQL = (parserNames: ParserName[]) => {
  const queryParserConfigured = new QueryParserFactory(parserNames, {}).getParser();

  return (query: string): Either<ParseFailure, Expression> => {
    const parsed = queryParserConfigured.parse(query);

    return parsed.status ?
      Right<ParseFailure, Expression>(parsed.value) :
      Left<ParseFailure, Expression>(ParseFailure.fromFailure(parsed as Failure, query));
  };
};
