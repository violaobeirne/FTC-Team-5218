package opmodes.LM0;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/5/2019.
 */
public class MozartSkybridgePath {
    public enum AllianceColor {
        BLUE,
        RED,
        DEFAULT;
    }

    public enum StartingPosition {
        BUILDING,
        LOADING,
        DEFAULT;
    }

    DeadReckonPath[][] paths = new DeadReckonPath[2][2];

    public MozartSkybridgePath() {
        // BLUE ALLIANCE, BUILDING SIDE
        paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30, 0.2); //forward to latch, 30
        paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 35.25, 0.2); //right, -35.25
        //paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30

        // BLUE ALLIANCE, LOADING SIDE
        paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.2); //forward, 20
        paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, -62, 0.2); //left to latch, -62
        //paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30

        // RED ALLIANCE, BUILDING SIDE
        paths[MozartSkybridgePath.AllianceColor.RED.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[MozartSkybridgePath.AllianceColor.RED.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30, 0.2); //forward, 30
        paths[MozartSkybridgePath.AllianceColor.RED.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 35.25, -0.2); //left, 35.25
        //paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30

        // RED ALLIANCE, LOADING SIDE
        paths[MozartSkybridgePath.AllianceColor.RED.ordinal()][MozartSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[MozartSkybridgePath.AllianceColor.RED.ordinal()][MozartSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.2); //forward, 20
        paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 62, 0.2); //right, 62
        //paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30
    }

    public DeadReckonPath getPath(MozartSkybridgePath.AllianceColor allianceColor, MozartSkybridgePath.StartingPosition startingPosition) {
        return paths[allianceColor.ordinal()][startingPosition.ordinal()];
    }
}
