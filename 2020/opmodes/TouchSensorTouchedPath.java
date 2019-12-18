package opmodes;

import team25core.DeadReckonPath;

public class TouchSensorTouchedPath {
    public enum AllianceColor {
        BLUE,
        RED,
        DEFAULT;
    }

    public enum StartingPosition {
        TOUCHING;
    }

    DeadReckonPath[][] paths = new DeadReckonPath[2][2];

    public TouchSensorTouchedPath(){
        paths[TouchSensorTouchedPath.AllianceColor.BLUE.ordinal()] [TouchSensorTouchedPath.StartingPosition.TOUCHING.ordinal()] = new DeadReckonPath();
        paths[TouchSensorTouchedPath.AllianceColor.BLUE.ordinal()] [TouchSensorTouchedPath.StartingPosition.TOUCHING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 36, 0.8);

        paths[TouchSensorTouchedPath.AllianceColor.RED.ordinal()] [TouchSensorTouchedPath.StartingPosition.TOUCHING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 36, 0.8);
    }

}
