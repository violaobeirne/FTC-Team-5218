package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.util.RobotLog;

import team25core.MotorStallTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.SingleShotTimerTask;

@Autonomous(name = "MotorStallTest")
//@Disabled
public class MotorStallTest extends Robot {

    private DcMotor lift;
    private MotorStallTask mst;

    /**
     * The default event handler for the robot.
     */
    @Override
    public void handleEvent(RobotEvent e)
    {
        MotorStallTask.MotorStallEvent event = (MotorStallTask.MotorStallEvent)e;
        if (event.kind == MotorStallTask.EventKind.STALLED) {
            RobotLog.i("Stalled");
            lift.setPower(0.0);
            mst.stop();
        }
    }

    @Override
    public void init()
    {
        lift = hardwareMap.get(DcMotor.class, "lift");
        lift.setMode(DcMotor.RunMode.STOP_AND_RESET_ENCODER);
        lift.setMode(DcMotor.RunMode.RUN_USING_ENCODER);
    }

    @Override
    public void start()
    {
        RobotLog.i("Start motor");

        lift.setPower(0.5);
        mst = new MotorStallTask(this, lift, telemetry);
        this.addTask(mst);
    }
}

