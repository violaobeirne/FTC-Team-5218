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
        paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.2);

        // BLUE ALLIANCE, LOADING SIDE
        paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[MozartSkybridgePath.AllianceColor.BLUE.ordinal()][MozartSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.2);

        // RED ALLIANCE, BUILDING SIDE
        paths[MozartSkybridgePath.AllianceColor.RED.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[MozartSkybridgePath.AllianceColor.RED.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 100, 0.2);
        // paths[MozartSkybridgePath.AllianceColor.RED.ordinal()][MozartSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 90, 0.2);

        // RED ALLIANCE, LOADING SIDE
        paths[MozartSkybridgePath.AllianceColor.RED.ordinal()][MozartSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[MozartSkybridgePath.AllianceColor.RED.ordinal()][MozartSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.2);
    }

    public DeadReckonPath getPath(MozartSkybridgePath.AllianceColor allianceColor, MozartSkybridgePath.StartingPosition startingPosition) {
        return paths[allianceColor.ordinal()][startingPosition.ordinal()];
    }
}
