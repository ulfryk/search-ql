import { List } from 'immutable';
import { Maybe, None } from 'monet';

import { NodeEvaluation, ValueType } from '../../../common/model';
import { FunctionRuntime } from '../../../common/runtimes';
import { RequiredFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

const lengthRuntime: FunctionRuntime<number> = (values, ast) => args =>
  NodeEvaluation.ofNumber(values, ast)(
    Maybe.fromNull(args.first())
      .map(getMatch => getMatch().value)
      .flatMap(fieldName => Maybe.fromNull(values.get(fieldName)))
      .fold(0)(value => value.length));

export const lengthFunction =
  new FunctionConfig(
    'length',
    List([RequiredFunctionArg.fromType(ValueType.Text, 'field_name')]),
    None(),
    ValueType.Number,
    lengthRuntime);
