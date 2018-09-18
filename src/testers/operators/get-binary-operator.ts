import { Map } from 'immutable';

import { AndOperator, BinaryOperator, GteOperator, GtOperator, IsNotOperator, IsOperator, LikeOperator, LteOperator, LtOperator, NotLikeOperator, OrOperator } from '../../ast';
import { BinaryOperatorRuntime } from '../../common/runtimes';
import { ParserConfig } from '../../config';
import { and } from './and';
import { gt } from './gt';
import { gte } from './gte';
import { is } from './is';
import { isNot } from './is-not';
import { like } from './like';
import { lt } from './lt';
import { lte } from './lte';
import { notLike } from './not-like';
import { or } from './or';

const getRuntimesA = ({ AND, IS, IS_NOT, LIKE, NOT_LIKE, OR }: ParserConfig) =>
  Map<BinaryOperator, BinaryOperatorRuntime<any, any, any>>([
    ...AND.map(token => [new AndOperator(token), and]),
    ...IS_NOT.map(token => [new IsNotOperator(token), isNot]),
    ...IS.map(token => [new IsOperator(token), is]),
    ...LIKE.map(token => [new LikeOperator(token), like]),
    ...NOT_LIKE.map(token => [new NotLikeOperator(token), notLike]),
    ...OR.map(token => [new OrOperator(token), or]),
  ] as any);

const getRuntimesB = ({ GT, GTE, LT, LTE }: ParserConfig) =>
  Map<BinaryOperator, BinaryOperatorRuntime<any, any, any>>([
    ...GTE.map(token => [new GteOperator(token), gte]),
    ...GT.map(token => [new GtOperator(token), gt]),
    ...LTE.map(token => [new LteOperator(token), lte]),
    ...LT.map(token => [new LtOperator(token), lt]),
  ] as any);

export const getBinaryOperatorRuntime =
  <L, R, V>(operator: BinaryOperator, config: ParserConfig): BinaryOperatorRuntime<L, R, V> => {
    const runtimesA = getRuntimesA(config);
    const runtimesB = getRuntimesB(config);
    // tslint:disable-next-line:strict-boolean-expressions
    return runtimesA.get(operator) || runtimesB.get(operator); // Immutable Map fails if too big…
  };
