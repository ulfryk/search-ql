import { None, Some } from 'monet';

import { Expression } from '../common/model';
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
            .map<Expression>(group.append(rhs, config)))
          .orElse(Some(BinaryOperationChain.init(rhs))),
        None<BinaryOperationChain>())
      .orJust(InvalidExpression.empty('No valid expression found.'));
