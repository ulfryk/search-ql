import { Map } from 'immutable';
import { Maybe } from 'monet';

import { Match } from '../../../index';

import { FunctionRuntime, NodeEvaluation } from '../../model';

export const isEmptyRuntime: FunctionRuntime<boolean> = (values, ast) => args => {
  const field = Maybe.fromNull(args.first())
    .map(getMatch => getMatch().value);
  const isEmpty = field
    .flatMap(fieldName => Maybe.fromNull(values.get(fieldName)))
    .flatMap(someValue => Maybe.fromFalsy(someValue.trim()))
    .isNone();

  return NodeEvaluation.ofBoolean(values, ast)(
    isEmpty,
    () => field.filter(() => isEmpty).map(fieldName => Map({ [fieldName]: Match.whole('') })));
};
