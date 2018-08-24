import { OrderedMap } from 'immutable';
import { Maybe, None, Some } from 'monet';
import * as P from 'parsimmon';

import { Expression, fromPairs, NotExpression } from '../expressions';
import { SyntaxConfig } from '../syntax-config';
import { basicExpression } from './basic';
import { labelledExpression } from './labelled';
import { logicalOperator } from './logical-operator';
import { ParserName } from './names';

export class QueryParserFactory {

  private readonly parserMappings = OrderedMap<ParserName, () => P.Parser<any>>([
    [ParserName.JoinedGroup, () => this.joinedGroup],
    [ParserName.Labelled, () => labelledExpression(this.config)],
    [ParserName.Not, () => this.notExpression],
    [ParserName.Basic, () => basicExpression(this.config)],
  ]);

  private readonly operator = P.whitespace
    .then(logicalOperator(this.config))
    .skip(P.whitespace);

  constructor(
    public readonly parserNames: ParserName[],
    public readonly config: SyntaxConfig = new SyntaxConfig(),
  ) {}

  public getParser(): P.Parser<Expression> {
    return P.alt(this.joinedExpression, this.subQuery);
  }

  private getParsers(): P.Parser<any>[] {
    return this.parserMappings
      .filter((__, name) => this.parserNames.includes(name))
      .map(getParser => getParser())
      .toArray();
  }

  private get subQuery(): P.Parser<Expression> {
    return P.lazy(() => P.alt(...this.getParsers()));
  }

  private get queryLogicalPart() {
    return P.seqMap(this.operator, this.subQuery, (op, expression) => [Some(op), expression]);
  }

  private get joinedExpression(): P.Parser<Expression> {
    return P.seqMap(
      this.subQuery,
      this.queryLogicalPart.many(),
      (head, tail) => [[None<string>(), head]].concat(tail),
    ).map((parser: [Maybe<string>, Expression][]) => fromPairs(parser, this.config));
  }

  private get joinedGroup(): P.Parser<Expression> {
    return P.seqMap(
      P.string(this.config.GROUP_START).skip(P.optWhitespace),
      this.joinedExpression,
      P.optWhitespace.then(P.string(this.config.GROUP_END)),
      (_start, logical, _end) => logical);
  }

  private get notExpression(): P.Parser<Expression> {
    return P.seqMap(
      P.string(this.config.NOT).skip(P.whitespace),
      this.subQuery,
      (_operator, logical) => new NotExpression(logical));
  }

}
