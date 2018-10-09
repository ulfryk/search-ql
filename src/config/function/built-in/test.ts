import { List } from 'immutable';
import { Right, Some } from 'monet';

import { ValueType } from '../../../common/model';
import { OptionalFunctionArg } from '../function-arg';
import { FunctionConfig } from '../function-config';

export const testFunction =
  new FunctionConfig(
    'test_function',
    List(),
    Some(OptionalFunctionArg.fromType(ValueType.Text, 'rest')),
    Right(ValueType.Boolean));
