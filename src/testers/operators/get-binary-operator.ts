import { Map } from 'immutable';

import { BinaryOperator } from '../../ast';
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

const getRuntimes = ({ AND, GT, GTE, IS, IS_NOT, LT, LTE, LIKE, NOT_LIKE, OR }: ParserConfig) =>
  Map<string, BinaryOperatorRuntime<any, any, any>>([
    ...AND.map(token => [token, and]),
    ...IS_NOT.map(token => [token, isNot]),
    ...IS.map(token => [token, is]),
    ...LIKE.map(token => [token, like]),
    ...NOT_LIKE.map(token => [token, notLike]),
    ...OR.map(token => [token, or]),
    ...GTE.map(token => [token, gte]),
    ...GT.map(token => [token, gt]),
    ...LTE.map(token => [token, lte]),
    ...LT.map(token => [token, lt]),
  ] as any);

export const getBinaryOperatorRuntime =
  <L, R, V>({ token }: BinaryOperator, config: ParserConfig): BinaryOperatorRuntime<L, R, V> =>
    getRuntimes(config).get(token);
