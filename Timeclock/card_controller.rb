class CardController < ApplicationController
  model :clock_user, :time_slot

  def view
    username = session['user']
    @user = ClockUser.find_by_name(username)

    if @user.nil?
      @user = ClockUser.new
      @user.name = username
      @user.save
    end

    session[:user_id] = @user.id
    @realname = session['commonname']
    @jobtitle = session['title']

    this_week = @user.time_slots.find(:all,
                  :conditions => ['start >= ?', Time.now.monday])

    seconds = 0
    this_week.each do |slot|
      seconds = seconds + (slot.finish - slot.start)
    end

    @hours_this_week = (seconds / 1.hours).floor
    seconds = seconds - (@hours_this_week * 1.hours)
    @minutes_this_week = (seconds / 1.minutes).floor

    # Paging - make sure we redirect to the last page

    @slot_pages, @slots = paginate(:time_slots,
                                   :per_page => 14,
                                   :conditions => "clock_user_id = #{@user.id}",
                                   :order_by => 'finish')

    if params[:page].nil?
       @slot_pages.current_page = @slot_pages.last
       redirect_to(:action => 'view', :page => @slot_pages.current )
    end
  end

  def clockin
    @user = ClockUser.find(session[:user_id])
    
    if @user.nil?
      flash[:notice] = 'not logged in'
      redirect_to(:action => 'view')
    end

    unless @user.in_time.nil?
      flash[:notice] = 'already clocked in'
    end

    @user.in_time = Time.now
    @user.save
    redirect_to(:action => 'view')
  end

  def clockout
    @user = ClockUser.find(session[:user_id])

    if @user.nil?
      flash[:notice] = 'not logged in'
      redirect_to(:action => 'view')
    end

    if @user.in_time.nil?
      flash[:notice] = 'not clocked in'
    end

    @user.time_slots << TimeSlot.new(:start  => @user.in_time, 
                                     :finish => Time.now)
    @user.in_time = nil
    @user.save
    redirect_to(:action => 'view')
  end

  def edit
    @slot = TimeSlot.find(params[:id])
  end

  def update
    @slot = TimeSlot.find(params[:id])
    if @slot.update_attributes(params[:slot])
      flash[:notice] = 'card entry updated'
      redirect_to(:action => 'view')
    else
      render :action => 'edit'
    end
  end

end
