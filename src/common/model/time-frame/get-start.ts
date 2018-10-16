import { ParsedComponents } from 'chrono-node';
import { startOfDay, startOfHour, startOfMinute, startOfMonth, startOfSecond, startOfYear } from 'date-fns';

// tslint:disable-next-line:cyclomatic-complexity
export const getStart = (components: ParsedComponents): Date => {
  if (!components.isCertain('year')) {
    throw new Error('A date without a year is not a date (at getStart function).');
  }

  if (!components.isCertain('month')) {
    return startOfYear(new Date(components.get('year')));
  }

  if (!components.isCertain('day')) {
    return startOfMonth(new Date(
      components.get('year'),
      components.get('month') - 1));
  }

  if (!components.isCertain('hour')) {
    return startOfDay(new Date(
      components.get('year'),
      components.get('month') - 1,
      components.get('day')));
  }

  if (!components.isCertain('minute')) {
    return startOfHour(new Date(
      components.get('year'),
      components.get('month') - 1,
      components.get('day'),
      components.get('hour')));
  }

  if (!components.isCertain('second')) {
    return startOfMinute(new Date(
      components.get('year'),
      components.get('month') - 1,
      components.get('day'),
      components.get('hour'),
      components.get('minute')));
  }

  if (!components.isCertain('millisecond')) {
    return startOfSecond(new Date(
      components.get('year'),
      components.get('month') - 1,
      components.get('day'),
      components.get('hour'),
      components.get('minute'),
      components.get('second')));
  }

  return components.date();
};
