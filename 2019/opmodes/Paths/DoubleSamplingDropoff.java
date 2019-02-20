package opmodes.Paths;

import opmodes.Utilities.MineralUtils;
import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/27/2018.
 */
public class DoubleSamplingDropoff {

    DeadReckonPath[][] paths = new DeadReckonPath[2][3];

    public DoubleSamplingDropoff() {
        // true paths tested on Monday
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.5);

        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()] = new DeadReckonPath();
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.5);

        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.5);

        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()] = new DeadReckonPath();
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.CENTER.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.5);

        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[MineralUtils.DropMarker.TRUE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.5);

        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
        paths[MineralUtils.DropMarker.FALSE.ordinal()][MineralUtils.MineralPosition.RIGHT.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 8, -0.5);
    }

    public DeadReckonPath getPath(MineralUtils.DropMarker markerDrop, MineralUtils.MineralPosition goldPosition)
    {
        return paths[markerDrop.ordinal()][goldPosition.ordinal()];
    }
}
