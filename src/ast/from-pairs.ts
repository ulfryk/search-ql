import { None, Some } from 'monet';

import { SyntaxConfig } from '../config';
import { BinaryOperationChain } from './binary-operation-chain';
import { Expression, InvalidExpression } from './expressions';
import { OOPair } from './oopair';
import { Operator } from './operators';

export const fromPairs =
  (pairs: OOPair[], config: SyntaxConfig): Expression =>
    pairs
      .reduce(
        (acc, [token, rhs]) => acc
          .flatMap(group => token
            .map(Operator.fromToken(config))
            .map<Expression>(group.append(rhs, config)))
          .orElse(Some(BinaryOperationChain.init(rhs))),
        None<BinaryOperationChain>())
      .orJust(InvalidExpression.empty('No valid expression found.'));
