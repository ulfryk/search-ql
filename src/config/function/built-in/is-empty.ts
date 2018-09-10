import { List, Map } from 'immutable';
import { Maybe, None } from 'monet';

import { Match, NodeEvaluation, ValueType } from '../../../common/model';
import { FunctionRuntime } from '../../../common/runtimes';
import { RequiredFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

const isEmptyRuntime: FunctionRuntime<boolean> = (values, ast) => args => {
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

export const isEmptyFunction =
  new FunctionConfig(
    'is_empty',
    List([RequiredFunctionArg.fromType(ValueType.Text, 'field_name')]),
    None(),
    ValueType.Boolean,
    isEmptyRuntime);
