import { List } from 'immutable';
import { Maybe, None } from 'monet';

import { NodeEvaluation, ValueType } from '../../../common/model';
import { FunctionRuntime } from '../../../common/runtimes';
import { RequiredFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

const isEmptyRuntime: FunctionRuntime<boolean> = (values, ast) => args =>
  NodeEvaluation.ofBoolean(values, ast)(
    Maybe.fromNull(args.first())
      .map(getMatch => getMatch().value)
      .flatMap(fieldName => Maybe.fromNull(values.get(fieldName)))
      .flatMap(value => Maybe.fromFalsy(value.trim()))
      .isSome());

export const isEmptyFunction =
  new FunctionConfig(
    'is_empty',
    List([RequiredFunctionArg.fromType(ValueType.Text, 'field_name')]),
    None(),
    ValueType.Boolean,
    isEmptyRuntime);
