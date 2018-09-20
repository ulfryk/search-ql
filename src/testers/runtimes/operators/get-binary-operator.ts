import { Map } from 'immutable';

import { BinaryOperator, OperatorType } from '../../../index';

import { BinaryOperatorRuntime } from '../../model';

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

const runtimes =
  Map<OperatorType, BinaryOperatorRuntime<any, any, any>>({
    [OperatorType.And]: and,
    [OperatorType.Gt]: gt,
    [OperatorType.Gte]: gte,
    [OperatorType.IsNot]: isNot,
    [OperatorType.Is]: is,
    [OperatorType.Like]: like,
    [OperatorType.Lt]: lt,
    [OperatorType.Lte]: lte,
    [OperatorType.NotLike]: notLike,
    [OperatorType.Or]: or,
  });

export const getBinaryOperatorRuntime =
  <L, R, V>({ type }: BinaryOperator): BinaryOperatorRuntime<L, R, V> =>
  runtimes.get(type);
