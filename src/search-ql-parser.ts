import { Either, Left, Right } from 'monet';
import * as P from 'parsimmon';

import { validate } from './ast';
import { Expression, Failure, ParseFailure } from './common/model';
import { ParserConfig } from './config';
import { QueryParserFactory } from './parsers';
import { Tester } from './testers';

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
      Right<Failure[], Expression>(parsed.value).flatMap(validate) :
      Left<Failure[], Expression>([ParseFailure.fromFailure(parsed as P.Failure, query)]);
  }

  public toTester(
    parsed: Either<Failure[], Expression>,
  ): Either<Failure[], Tester<any, Expression, any>> {
    return parsed.map(Tester.fromAst(this.factory.config));
  }

}
