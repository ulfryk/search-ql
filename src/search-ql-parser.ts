import { Either, Left, Right } from 'monet';
import * as P from 'parsimmon';

import { validate } from './ast';
import { Expression, Failure, ParseFailure } from './common/model';
import { ParserConfig } from './config';
import { QueryParserFactory } from './parsers';

export class SearchQLParser {

  public static create(config: Partial<ParserConfig> = {}) {
    return new SearchQLParser(new QueryParserFactory(ParserConfig.create(config)));
  }

  constructor(
    public readonly factory: QueryParserFactory,
  ) {}

  public parse(query: string): Either<Failure[], Expression> {
    const parsed = this.factory.getParser().parse(query);

    return parsed.status ?
      Right<Failure[], Expression>(parsed.value).flatMap(validate(this.factory.config.model)) :
      Left([ParseFailure.fromFailure(parsed as P.Failure, query)]);
  }

}
