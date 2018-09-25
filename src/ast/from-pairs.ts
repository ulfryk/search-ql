import { None, Some } from 'monet';

import { Expression, ParseFailure } from '../common/model';
import { ParserConfig } from '../config';
import { BinaryOperationChain } from './binary-operation-chain';
import { InvalidExpression } from './expressions';
import { OOPair } from './oopair';
import { Operator } from './operators';

export const fromPairs =
  (pairs: OOPair[], config: ParserConfig): Expression =>
    pairs
      .reduce(
        (acc, [token, rhs]) => acc
          .flatMap(group => token
            .map(Operator.fromToken(config))
            .map(group.append(rhs, config)))
          .orElse(Some(BinaryOperationChain.init(rhs))),
        None<BinaryOperationChain>())
      .map(e => e as Expression)
      .orJust(InvalidExpression
        .empty(new ParseFailure('No valid expression found.')));
