import { Either, Left, Right } from 'monet';
import { Failure } from 'parsimmon';

import { Expression } from './ast';
import { SyntaxConfig } from './config';
import { ParseFailure } from './parse-failure';
import { ParserName, QueryParserFactory } from './parsers';

export const parseSearchQL = (parserNames: ParserName[], config?: SyntaxConfig) => {
  const queryParserConfigured = new QueryParserFactory(parserNames, config).getParser();

  return (query: string): Either<ParseFailure, Expression> => {
    const parsed = queryParserConfigured.parse(query);

    return parsed.status ?
      Right<ParseFailure, Expression>(parsed.value.reshape().checkTypes()) :
      Left<ParseFailure, Expression>(ParseFailure.fromFailure(parsed as Failure, query));
  };
};
