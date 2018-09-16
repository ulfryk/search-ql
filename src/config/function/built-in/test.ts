import { List } from 'immutable';
import { Some } from 'monet';

import { NodeEvaluation, ValueType } from '../../../common/model';
import { FunctionRuntime } from '../../../common/runtimes';
import { OptionalFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

const testRuntime: FunctionRuntime<boolean> = (values, ast) => () =>
  NodeEvaluation.ofBoolean(values, ast)(false);

export const testFunction =
  new FunctionConfig(
    'test_function',
    List(),
    Some(OptionalFunctionArg.fromType(ValueType.Text, 'rest')),
    ValueType.Boolean,
    testRuntime);
